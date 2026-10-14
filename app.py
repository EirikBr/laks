# -*- coding: utf-8 -*-
"""Laksestatistikk: ukentlig lakseeksport (SSB 03024) + fylkeseksport (SSB 08817) + valuta (Norges Bank)."""
import math

import pandas as pd
from flask import Flask, Response, jsonify, render_template, request

import ssb

app = Flask(__name__)
app.json.ensure_ascii = False

VALUTA = ("NOK", "EUR", "USD")


def _ints(name, default=None):
    raw = request.args.get(name, "")
    vals = [int(x) for x in raw.split(",") if x.strip().isdigit()]
    return vals or default


def _list(name):
    return [x for x in request.args.get(name, "").split("|") if x]


def _records(df: pd.DataFrame) -> list:
    return [{k: (None if isinstance(v, float) and math.isnan(v) else v) for k, v in r.items()}
            for r in df.to_dict(orient="records")]


def _csv(df: pd.DataFrame, filename: str) -> Response:
    return Response(df.to_csv(index=False, sep=";", decimal=",").encode("utf-8-sig"),
                    mimetype="text/csv; charset=utf-8",
                    headers={"Content-Disposition": f"attachment; filename={filename}"})


# ---------- Laks (03024) ----------

def laks_aggregat(laks: pd.DataFrame, uker, fra, til, valuta) -> pd.DataFrame:
    f = laks[(laks["aar"] >= fra) & (laks["aar"] <= til)]
    if uker:
        f = f[f["uke"].isin(uker)]
    f = f.copy()
    kurs = f[valuta] if valuta != "NOK" else 1.0
    f["fersk_verdi_v"] = f["fersk_verdi"] / kurs
    f["frosen_verdi_v"] = f["frosen_verdi"] / kurs
    agg = f.groupby("aar", as_index=False).agg(
        uker=("uke", "count"),
        fersk_tonn=("fersk_tonn", "sum"),
        frosen_tonn=("frosen_tonn", "sum"),
        fersk_verdi=("fersk_verdi_v", "sum"),
        frosen_verdi=("frosen_verdi_v", "sum"),
    )
    # Volumvektet snittpris (riktigere enn snitt av ukepriser)
    agg["fersk_pris"] = agg["fersk_verdi"] / agg["fersk_tonn"] / 1000
    agg["frosen_pris"] = agg["frosen_verdi"] / agg["frosen_tonn"].where(agg["frosen_tonn"] > 0) / 1000
    return agg.round({"fersk_pris": 2, "frosen_pris": 2, "fersk_verdi": 0, "frosen_verdi": 0})


def laks_parametre(laks):
    aar_min, aar_max = int(laks["aar"].min()), int(laks["aar"].max())
    valuta = request.args.get("valuta", "NOK")
    valuta = valuta if valuta in VALUTA else "NOK"
    return _ints("uker", []), _ints("fra", [aar_min])[0], _ints("til", [aar_max])[0], valuta


@app.get("/api/laks")
def api_laks():
    d = ssb.get_data()
    laks = d["laks"]
    uker, fra, til, valuta = laks_parametre(laks)
    agg = laks_aggregat(laks, uker, fra, til, valuta)

    # Sesongprofil: kilopris fersk per uke for de siste 5 årene
    siste = sorted(laks["aar"].unique())[-5:]
    s = laks[laks["aar"].isin(siste)].copy()
    s["pris"] = s["fersk_pris"] / (s[valuta] if valuta != "NOK" else 1.0)
    sesong = {int(a): {"uke": g["uke"].tolist(), "pris": g["pris"].round(2).tolist()}
              for a, g in s.groupby("aar")}
    return jsonify(aggregat=_records(agg), sesong=sesong, valuta=valuta)


@app.get("/laks.csv")
def laks_csv():
    laks = ssb.get_data()["laks"]
    uker, fra, til, valuta = laks_parametre(laks)
    agg = laks_aggregat(laks, uker, fra, til, valuta).rename(columns={
        "aar": "År", "uker": "Antall uker",
        "fersk_tonn": "Fersk tonn", "frosen_tonn": "Frosen tonn",
        "fersk_verdi": f"Fersk verdi ({valuta})", "frosen_verdi": f"Frosen verdi ({valuta})",
        "fersk_pris": f"Fersk kilopris ({valuta})", "frosen_pris": f"Frosen kilopris ({valuta})",
    })
    return _csv(agg, "laks_03024.csv")


# ---------- Fylkeseksport (08817) ----------

def fylke_filtrert(fylke, regioner, varegrupper, fra, til, hittil):
    f = fylke[fylke["region"].isin(regioner) & fylke["varegruppe"].isin(varegrupper)
              & (fylke["aar"] >= fra) & (fylke["aar"] <= til)]
    if hittil:  # samme måneder som siste tilgjengelige år, så året i år kan sammenlignes
        siste_aar = fylke["aar"].max()
        siste_mnd = fylke.loc[fylke["aar"] == siste_aar, "mnd"].max()
        f = f[f["mnd"] <= siste_mnd]
    return f.groupby(["aar", "region", "varegruppe"], as_index=False)["mill_kr"].sum()


def fylke_parametre(fylke):
    return (_list("regioner") or ["Hele landet"], _list("varegrupper") or ["Fisk"],
            _ints("fra", [2010])[0], _ints("til", [int(fylke["aar"].max())])[0],
            request.args.get("periode") == "hittil")


@app.get("/api/fylke")
def api_fylke():
    fylke = ssb.get_data()["fylke"]
    return jsonify(rader=_records(fylke_filtrert(fylke, *fylke_parametre(fylke))))


@app.get("/fylke.csv")
def fylke_csv():
    fylke = ssb.get_data()["fylke"]
    df = fylke_filtrert(fylke, *fylke_parametre(fylke)).rename(columns={
        "aar": "År", "region": "Region", "varegruppe": "Varegruppe", "mill_kr": "Mill. kr"})
    return _csv(df, "fylkeseksport_08817.csv")


# ---------- Side ----------

@app.get("/")
def index():
    d = ssb.get_data()
    laks, fylke = d["laks"], d["fylke"]
    siste = laks.dropna(subset=["fersk_pris"]).iloc[-1]
    ifjor = laks[(laks["aar"] == siste["aar"] - 1) & (laks["uke"] == siste["uke"])]
    hittil = laks[(laks["aar"] == siste["aar"]) & (laks["uke"] <= siste["uke"])]
    hittil_ifjor = laks[(laks["aar"] == siste["aar"] - 1) & (laks["uke"] <= siste["uke"])]
    fylke_siste_aar = int(fylke["aar"].max())
    meta = {
        "siste_aar": int(siste["aar"]), "siste_uke": int(siste["uke"]),
        "pris": float(siste["fersk_pris"]), "tonn": float(siste["fersk_tonn"]),
        "pris_ifjor": float(ifjor["fersk_pris"].iloc[0]) if len(ifjor) else None,
        "eur": None if pd.isna(siste.get("EUR")) else float(siste["EUR"]),
        "verdi_hittil": float(hittil["fersk_verdi"].sum() + hittil["frosen_verdi"].sum()),
        "verdi_hittil_ifjor": float(hittil_ifjor["fersk_verdi"].sum() + hittil_ifjor["frosen_verdi"].sum()),
        "aar_min": int(laks["aar"].min()),
        "fylke_aar_min": int(fylke["aar"].min()), "fylke_aar_max": fylke_siste_aar,
        "fylke_siste_mnd": int(fylke.loc[fylke["aar"] == fylke_siste_aar, "mnd"].max()),
    }
    return render_template("index.html", meta=meta, regioner=d["regioner"],
                           varegrupper=list(ssb.VAREGRUPPE_NAVN.values()))


if __name__ == "__main__":
    app.run(debug=True, port=5004)
