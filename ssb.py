# -*- coding: utf-8 -*-
"""Datahenting og bearbeiding: SSB 03024 (ukentlig laks), SSB 08817 (fylkeseksport) og Norges Bank valuta."""
import io
import itertools
import threading
import time
from concurrent.futures import ThreadPoolExecutor

import pandas as pd
import requests

SSB_API = "https://data.ssb.no/api/v0/no/table/{}/"
NB_EXR = ("https://data.norges-bank.no/api/data/EXR/B.EUR+USD.NOK.SP"
          "?format=csv&startPeriod=2000-01-01&locale=en")
CACHE_TTL = 3600

# Gamle fylker slås sammen til dagens (2024-) fylker. Regionene fra 2020–2023 (Viken,
# Vestfold og Telemark, Troms og Finnmark) kan ikke splittes og beholdes som egne serier.
REGION_HARMONISERING = {
    "01": "31", "02": "32", "06": "33", "07": "39", "08": "40",
    "04": "34", "05": "34", "09": "42", "10": "42", "12": "46", "14": "46",
    "16": "50", "17": "50", "19": "55", "20": "56",
}
VAREGRUPPE_NAVN = {
    "SITC0-1": "Matvarer, drikkevarer, tobakk",
    "SITC03u": "Fisk",
    "MAT_EX_FISK": "Matvarer ekskl. fisk",
    "SITC2_4": "Råvarer, unntatt brensel",
    "SITC3b": "Brenselsstoffer",
    "SITC5-9": "Bearbeidde varer",
    "SITCT": "Varer i alt",
}


def _jsonstat2_to_df(js: dict) -> pd.DataFrame:
    """Gjør om JSON-stat 2.0 til lang dataframe med kode-kolonner (uten pyjstat-avhengighet)."""
    ids = js["id"]
    codes = []
    for dim in ids:
        idx = js["dimension"][dim]["category"]["index"]
        codes.append(sorted(idx, key=idx.get) if isinstance(idx, dict) else idx)
    values = js["value"]
    if isinstance(values, dict):  # sparse format
        dense = [None] * len(list(itertools.product(*codes)))
        for k, v in values.items():
            dense[int(k)] = v
        values = dense
    df = pd.DataFrame(list(itertools.product(*codes)), columns=ids)
    df["verdi"] = pd.to_numeric(pd.Series(values), errors="coerce")
    return df


def _labels(js: dict, dim: str) -> dict:
    return js["dimension"][dim]["category"]["label"]


def _ssb(table: str, query: list) -> dict:
    r = requests.post(SSB_API.format(table),
                      json={"query": query, "response": {"format": "json-stat2"}}, timeout=60)
    r.raise_for_status()
    return r.json()


# ---------- 03024: ukentlig laks ----------

def fetch_laks() -> pd.DataFrame:
    js = _ssb("03024", [])
    df = _jsonstat2_to_df(js)
    df["gruppe"] = df["VareGrupper2"].map({"01": "fersk", "02": "frosen"})
    df["mål"] = df["ContentsCode"].map({"Vekt": "tonn", "Kilopris": "pris"})
    wide = df.pivot_table(index="Tid", columns=["gruppe", "mål"], values="verdi", aggfunc="first")
    wide.columns = [f"{g}_{m}" for g, m in wide.columns]
    wide = wide.reset_index()
    wide["aar"] = wide["Tid"].str[:4].astype(int)
    wide["uke"] = wide["Tid"].str[5:7].astype(int)
    for g in ("fersk", "frosen"):
        wide[f"{g}_verdi"] = (wide[f"{g}_tonn"] * wide[f"{g}_pris"] * 1000).fillna(0)
    # Mandag i ISO-uka, brukes til valutakobling
    wide["dato"] = pd.to_datetime(wide["aar"].astype(str) + "-W" + wide["uke"].astype(str).str.zfill(2) + "-1",
                                  format="%G-W%V-%u", errors="coerce")
    return wide.drop(columns=["Tid"]).sort_values(["aar", "uke"]).reset_index(drop=True)


# ---------- Norges Bank: valuta ----------

def fetch_valuta() -> pd.DataFrame:
    """Daglige kurser -> snitt per ISO-uke (kolonner: aar, uke, EUR, USD)."""
    r = requests.get(NB_EXR, timeout=60)
    r.raise_for_status()
    df = pd.read_csv(io.StringIO(r.text), sep=";", dtype=str)
    df = df[["BASE_CUR", "TIME_PERIOD", "OBS_VALUE"]]
    df["OBS_VALUE"] = pd.to_numeric(df["OBS_VALUE"].str.replace(",", "."), errors="coerce")
    df["TIME_PERIOD"] = pd.to_datetime(df["TIME_PERIOD"])
    iso = df["TIME_PERIOD"].dt.isocalendar()
    df["aar"], df["uke"] = iso["year"].astype(int), iso["week"].astype(int)
    return (df.pivot_table(index=["aar", "uke"], columns="BASE_CUR", values="OBS_VALUE", aggfunc="mean")
              .reset_index().rename_axis(None, axis=1))


# ---------- 08817: fastlandseksport per fylke ----------

def fetch_fylke() -> tuple[pd.DataFrame, list]:
    js = _ssb("08817", [
        {"code": "Region", "selection": {"filter": "all", "values": ["*"]}},
        {"code": "SITC", "selection": {"filter": "all", "values": ["*"]}},
        {"code": "ContentsCode", "selection": {"filter": "item", "values": ["Verdi"]}},
    ])
    df = _jsonstat2_to_df(js).dropna(subset=["verdi"])
    region_navn = {k: v.replace(" (-2019)", "").replace(" (-2017)", "")
                   for k, v in _labels(js, "Region").items()}
    df["Region"] = df["Region"].replace(REGION_HARMONISERING)
    df = df.groupby(["Region", "SITC", "Tid"], as_index=False)["verdi"].sum(min_count=1)

    # Avledet varegruppe: matvarer ekskl. fisk
    mat = df[df["SITC"] == "SITC0-1"].set_index(["Region", "Tid"])["verdi"]
    fisk = df[df["SITC"] == "SITC03u"].set_index(["Region", "Tid"])["verdi"]
    ex = (mat - fisk.reindex(mat.index).fillna(0)).reset_index()
    ex["SITC"] = "MAT_EX_FISK"
    df = pd.concat([df, ex[["Region", "SITC", "Tid", "verdi"]]], ignore_index=True)

    df["aar"] = df["Tid"].str[:4].astype(int)
    df["mnd"] = df["Tid"].str[5:7].astype(int)
    df["region"] = df["Region"].map(region_navn)
    df["varegruppe"] = df["SITC"].map(VAREGRUPPE_NAVN)
    df = df.rename(columns={"verdi": "mill_kr"})[["region", "varegruppe", "aar", "mnd", "mill_kr"]]

    # Regionrekkefølge som hos SSB, uten de gamle kodene som er slått sammen
    regioner = [region_navn[k] for k in _labels(js, "Region") if k not in REGION_HARMONISERING]
    return df, regioner


# ---------- Cache ----------

_cache: dict = {"ts": 0.0}
_lock = threading.Lock()


def get_data() -> dict:
    with _lock:
        if time.time() - _cache["ts"] > CACHE_TTL:
            with ThreadPoolExecutor(3) as ex:
                f_laks, f_val, f_fylke = ex.submit(fetch_laks), ex.submit(fetch_valuta), ex.submit(fetch_fylke)
                laks = f_laks.result()
                try:
                    laks = laks.merge(f_val.result(), on=["aar", "uke"], how="left")
                except Exception:  # valuta er "nice to have"
                    laks["EUR"] = laks["USD"] = float("nan")
                fylke, regioner = f_fylke.result()
            _cache.update(ts=time.time(), laks=laks, fylke=fylke, regioner=regioner)
        return _cache
