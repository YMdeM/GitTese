import sys
from pathlib import Path as P
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
from matplotlib.patches import Circle, FancyBboxPatch, PathPatch, Rectangle
from matplotlib.path import Path
from matplotlib.lines import Line2D
from matplotlib.colors import LinearSegmentedColormap, TwoSlopeNorm


# ==============================================================================

PASTA = P(sys.argv[1]) if len(sys.argv) > 1 else P("Resultados_2026-11-11")
OUT = P(sys.argv[2]) if len(sys.argv) > 2 else PASTA / "graficos_python"
OUT.mkdir(parents=True, exist_ok=True)
(OUT / "demografia").mkdir(exist_ok=True)
(OUT / "exito_por_categoria").mkdir(exist_ok=True)

R = pd.read_excel(PASTA / "resultados_tese_2026-11.xlsx", sheet_name=None)
C = pd.read_excel(PASTA / "complemento_graficos_2026-11.xlsx", sheet_name=None)

plt.rcParams.update({
    "font.family": "serif", "font.serif": ["Times New Roman", "Liberation Serif"],
    "font.size": 10, "axes.edgecolor": "#c3c2b7", "axes.labelcolor": "#0b0b0b",
    "xtick.color": "#52514e", "ytick.color": "#0b0b0b", "axes.linewidth": 0.8,
    "savefig.dpi": 300, "savefig.bbox": "tight", "savefig.facecolor": "white",
    "axes.unicode_minus": True,
})
CM = 1 / 2.54
W = 16 * CM
AZUL, LARANJA, VERDE, VERDE_T = "#2a78d6", "#eb6834", "#1baf7a", "#13875e"
AZUL_C, LAR_C = "#9fc2ec", "#f5bba3"
CINZA, CINZA_C, TINTA, TINTA2, GRADE = "#a8a7a0", "#d9d8d2", "#0b0b0b", "#52514e", "#e1e0d9"
GRUPO_COR = {"Ingressantes": "#9fc2ec", "Egressos": AZUL, "Controles pareados (PNAD, GO)": LARANJA}


def br(x, d=1):
    return f"{x:.{d}f}".replace(".", ",").replace("-", "−")


def mil(n):
    return f"{int(n):,}".replace(",", ".")


def est(p):
    return "***" if p < .001 else "**" if p < .01 else "*" if p < .05 else ""


def limpa(ax, grade="x"):
    for s in ("top", "right", "left"):
        ax.spines[s].set_visible(False)
    if grade:
        getattr(ax, f"{grade}axis").grid(True, color=GRADE, lw=0.6)
        ax.set_axisbelow(True)
    ax.tick_params(axis="y", length=0)


def fmt_eixo(ax, eixo="x", d=0, pct=False):
    f = (lambda v, _: br(v, d) + "%") if pct else (lambda v, _: br(v, d))
    getattr(ax, f"{eixo}axis").set_major_formatter(mtick.FuncFormatter(f))


def salva(fig, nome, sub=None):
    fig.savefig((OUT / sub / nome) if sub else (OUT / nome))
    plt.close(fig)
    print("ok:", nome)


RENDA_RS = {"Baixa": "renda até 2 SM", "Média": "renda de 2 a 10 SM", "Alta": "renda acima de 10 SM"}
OCUP_RS = {"Ocupado": "Ocupado", "Desocupação": "Desocupado", "Inativo": "Inativo"}
GRUPOS = [
    ("Tipo de curso", "Tipo de curso (ref.: Qualificação)"),
    ("Modalidade", "Modalidade (ref.: Presencial)"),
    ("Turno", "Turno (ref.: Matutino)"),
    ("RS", "Ocupação e renda no ingresso (ref.: desocupado, renda até 2 SM)"),
    ("Ocupação no ingresso", "Ocupação no ingresso (ref.: desocupado)"),
    ("Transição", "Trajetória após o curso"),
    ("Socio", "Condição socioeconômica no ingresso"),
    ("Cor/raça", "Cor/raça (ref.: não branca)"),
    ("Perfil", "Perfil do aluno"),
    ("Moradia", "Moradia (ref.: urbana)"),
]
ESCALA = {"Idade": (10, "Idade (+10 anos)"), "Escolaridade": (1, "Escolaridade (+1 nível)"),
          "Renda": (1, "Renda no ingresso (+1 faixa)"), "Tempo de deslocamento": (1, "Tempo de deslocamento (+1 faixa)"),
          "Horas de trabalho": (1, "Horas de trabalho (+1 faixa)")}


def classifica(v):
    if v in ("Mulher (ref.: homem)", "Idade", "Escolaridade"):
        return "Perfil", ESCALA.get(v, (1, v))[1]
    if v.startswith("Inativo no ingresso"):
        return "Ocupação no ingresso", "Inativo"
    if ": " in v:
        g, c = v.split(": ", 1)
        if g == "RS":
            o, r = c.split(" + ")
            return "RS", f"{OCUP_RS[o]}, {RENDA_RS[r]}"
        c = {"Tecnico/Superior": "Técnico/Superior", "INTEGRAL": "Integral", "NOTURNO": "Noturno",
             "VESPERTINO": "Vespertino", "Não informado": "Não informada"}.get(c, c)
        return g, c
    if v.startswith("Transição"):
        return "Transição", v
    if v in ("Mulher (ref.: homem)", "Idade", "Escolaridade"):
        return "Perfil", ESCALA.get(v, (1, v))[1]
    return "Socio", ESCALA.get(v, (1, v))[1]


def prepara_ame(df):
    df = df[~df.Variavel.str.startswith("Ano de ingresso")].copy()
    k = df.Variavel.map(lambda v: ESCALA.get(v, (1, v))[0])
    for c in ("AME", "IC_inf", "IC_sup"):
        df[c] = df[c] * 100 * k
    df[["grupo", "item"]] = df.Variavel.apply(lambda v: pd.Series(classifica(v)))
    ordem = {g: i for i, (g, _) in enumerate(GRUPOS)}
    df["o1"] = df.grupo.map(ordem)
    ord_rs = ["Ocupado, renda até 2 SM", "Ocupado, renda de 2 a 10 SM", "Ocupado, renda acima de 10 SM",
              "Desocupado, renda de 2 a 10 SM", "Desocupado, renda acima de 10 SM", "Inativo, renda até 2 SM",
              "Inativo, renda de 2 a 10 SM", "Inativo, renda acima de 10 SM"]
    df["o2"] = [ord_rs.index(i) if i in ord_rs else k for k, i in enumerate(df["item"])]
    return df.sort_values(["o1", "o2"])


def cor_sig(r):
    return CINZA if r.p >= 0.05 else (AZUL if r.AME > 0 else LARANJA)


CURTO = {"Tipo de curso": "Tipo", "Ocupação no ingresso": "Ocupação"}


def rot_curto(v):
    g, c = classifica(v)
    if g == "RS":
        return "RS: " + c.replace("renda ", "")
    if g in ("Perfil", "Socio", "Transição"):
        return {"Mulher (ref.: homem)": "Mulher", "Idade (+10 anos)": "Idade (anos)",
                "Escolaridade (+1 nível)": "Escolaridade", "Renda no ingresso (+1 faixa)": "Renda (faixa)",
                "Tempo de deslocamento (+1 faixa)": "Tempo de deslocamento",
                "Horas de trabalho (+1 faixa)": "Horas de trabalho"}.get(c, c)
    return f"{CURTO.get(g, g)}: {c}"


ORDEM_CAT = {
    "Sexo": ["Mulher", "Homem"],
    "Faixa etária": ["Até 24 anos", "25 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos", "60 anos ou mais"],
    "Cor/raça": ["Branca", "Não branca", "Não informado"],
    "Escolaridade": ["Sem instrução", "Fundamental incompleto", "Fundamental completo", "Médio incompleto",
                     "Médio completo", "Superior incompleto", "Superior completo"],
    "Moradia": ["Urbana", "Rural"],
    "Renda no ingresso": ["Sem renda", "Até 1 SM", "Mais de 1 a 2 SM", "Mais de 2 a 5 SM", "Mais de 5 a 7 SM",
                          "Mais de 7 a 10 SM", "Mais de 10 SM"],
    "Situação ocupacional no ingresso": ["Ocupado", "Desocupado", "Inativo"],
    "Tipo de curso": ["Capacitação", "Qualificação", "Técnico/Superior"],
}
ARQ = {"Sexo": "sexo", "Faixa etária": "faixa_etaria", "Cor/raça": "cor_raca", "Escolaridade": "escolaridade",
       "Moradia": "moradia", "Renda no ingresso": "renda", "Situação ocupacional no ingresso": "ocupacao",
       "Tipo de curso": "tipo_curso"}



# ==============================================================================

# --- Fluxo de construção das amostras ---
fl = R["T1_fluxo_amostra"]
fig, axs = plt.subplots(2, 1, figsize=(W, 12 * CM), gridspec_kw={"hspace": 0.45,
                        "height_ratios": [len(fl[fl.Etapa == "Ingressantes"]), len(fl[fl.Etapa == "Egressos"])]})
for ax, (etapa, sub) in zip(axs, fl.groupby("Etapa", sort=False)):
    sub = sub.reset_index(drop=True)
    y = np.arange(len(sub))[::-1]
    ax.barh(y, sub.N, color=[AZUL if i == len(sub) - 1 else AZUL_C for i in range(len(sub))], height=0.6, zorder=2)
    for yi, (i, r) in zip(y, sub.iterrows()):
        perda = "" if i == 0 else f"  (−{mil(sub.N[i - 1] - r.N)})"
        ax.text(r.N + sub.N.max() * 0.01, yi, mil(r.N) + perda, va="center", fontsize=8.5)
    ax.set_yticks(y, sub.Passo, fontsize=9)
    ax.set_xlim(0, sub.N.max() * 1.25)
    ax.set_title(etapa, loc="left", fontsize=10.5, fontweight="bold")
    ax.xaxis.set_major_formatter(mtick.FuncFormatter(lambda v, _: mil(v)))
    limpa(ax)
salva(fig, "N01_fluxo_amostra.png")



# ==============================================================================

def forest_ame(df, xlabel):
    nomes = dict(GRUPOS)
    linhas = []
    for g, sub in df.groupby("grupo", sort=False):
        linhas.append(("h", nomes[g], None))
        for _, r in sub.iterrows():
            linhas.append(("i", r["item"], r))
    n = len(linhas)
    fig, ax = plt.subplots(figsize=(W, (n * 0.52 + 1.6) * CM))
    lo = min(df.IC_inf.min(), 0)
    hi = max(df.IC_sup.max(), 0)
    pad = (hi - lo) * 0.16
    for i, (tipo, txt, r) in enumerate(linhas):
        y = n - 1 - i
        if tipo == "h":
            ax.text(-0.01, y - 0.1, txt, transform=ax.get_yaxis_transform(), ha="right", va="center",
                    fontweight="bold", fontsize=9.5)
            continue
        ax.text(-0.01, y, txt, transform=ax.get_yaxis_transform(), ha="right", va="center", fontsize=9.5,
                color=TINTA2)
        c = cor_sig(r)
        ax.plot([r.IC_inf, r.IC_sup], [y, y], color=c, lw=1.6, solid_capstyle="round", zorder=2)
        ax.scatter(r.AME, y, s=26, color=c, edgecolor="white", lw=0.8, zorder=3)
        lado = 1 if r.AME >= 0 else -1
        xt = (r.IC_sup if lado > 0 else r.IC_inf) + lado * (hi - lo) * 0.012
        ax.text(xt, y, br(r.AME) + est(r.p), ha="left" if lado > 0 else "right", va="center", fontsize=8.5,
                color=TINTA if r.p < 0.05 else CINZA)
    ax.axvline(0, color=TINTA2, lw=0.8)
    ax.set_xlim(lo - pad, hi + pad)
    ax.set_ylim(-0.7, n - 0.3)
    ax.set_yticks([])
    ax.set_xlabel(xlabel)
    limpa(ax)
    fmt_eixo(ax)
    return fig


ame1 = R["E1_AME_CM1_CM5"]
for m in ("CM5", "CM1"):
    fig = forest_ame(prepara_ame(ame1[ame1.Modelo == m]),
                     "Efeito marginal médio sobre a probabilidade de êxito (pontos percentuais, IC 95%)")
    salva(fig, f"G02_ame_{m.lower()}.png")


mb = R["E1_modelos_binarios"]
mb = mb[~mb.Variavel.str.startswith("Ano de ingresso")]
mods = ["CM1", "CM3", "CM4", "CM5", "CM5c", "CM6"]
mb = mb[mb.Modelo.isin(mods)]
ordem_v = list(dict.fromkeys(prepara_ame(mb.assign(AME=0, IC_inf=0, IC_sup=0)).Variavel))
M = mb.pivot_table(index="Variavel", columns="Modelo", values="OR").reindex(index=ordem_v, columns=mods)
Pv = mb.pivot_table(index="Variavel", columns="Modelo", values="p").reindex(index=ordem_v, columns=mods)
cmap = LinearSegmentedColormap.from_list("div", [LARANJA, "#f6c3ab", "#f0efec", "#a9cbf2", AZUL])
cap = np.log(5)
fig, ax = plt.subplots(figsize=(W, (len(M) * 0.55 + 2.4) * CM))
for i, v in enumerate(M.index):
    for j, m in enumerate(mods):
        o = M.loc[v, m]
        if np.isnan(o):
            ax.add_patch(Rectangle((j, i), 1, 1, facecolor="white", edgecolor=GRADE, lw=0.4, hatch="////"))
            continue
        p = Pv.loc[v, m]
        val = np.clip(np.log(o), -cap, cap) if p < 0.05 else 0
        ax.add_patch(Rectangle((j, i), 1, 1, facecolor=cmap(TwoSlopeNorm(0, -cap, cap)(val)), edgecolor="white", lw=1.5))
        txt = "white" if abs(val) > np.log(3) else (TINTA if p < 0.05 else "#898781")
        ax.text(j + 0.5, i + 0.52, br(o, 2) + est(p), ha="center", va="center", fontsize=8, color=txt)
ax.set_xlim(0, len(mods))
ax.set_ylim(len(M), 0)
ax.set_xticks(np.arange(len(mods)) + 0.5, mods)
ax.xaxis.tick_top()
ax.set_yticks(np.arange(len(M)) + 0.5, [rot_curto(v) for v in M.index])
ax.tick_params(length=0)
for s in ax.spines.values():
    s.set_visible(False)
leg = [Rectangle((0, 0), 1, 1, color=AZUL), Rectangle((0, 0), 1, 1, color=LARANJA),
       Rectangle((0, 0), 1, 1, color="#f0efec"), Rectangle((0, 0), 1, 1, facecolor="white", edgecolor=GRADE, hatch="////")]
ax.legend(leg, ["Aumenta a chance de êxito (RC > 1)", "Reduz a chance de êxito (RC < 1)",
                "Não significativo (p ≥ 0,05)", "Variável fora do modelo"],
          loc="upper center", bbox_to_anchor=(0.42, -0.01), ncol=2, frameon=False, fontsize=8.5)
salva(fig, "G03_heatmap_modelos_exito.png")


sh = R["E1_H1b_Shapley_pseudoR2"]
rot_b = {"Institucionais": "Institucionais", "Socioeconomicas": "Socioeconômicas", "Demograficas": "Demográficas"}
cores_b = {"Institucionais": AZUL, "Socioeconomicas": LARANJA, "Demograficas": VERDE}
fig, ax = plt.subplots(figsize=(W, 4.2 * CM))
x0 = 0
for _, r in sh.iterrows():
    w = r.Participacao * 100
    ax.barh(0, w, left=x0, color=cores_b[r.Bloco], height=0.55, edgecolor="white", lw=1.5)
    if w > 4:
        ax.text(x0 + w / 2, 0, f"{br(w)}%", ha="center", va="center", color="white", fontsize=9.5, fontweight="bold")
    x0 += w
ax.set_xlim(0, 100)
ax.set_yticks([])
fmt_eixo(ax, pct=True)
ax.set_xlabel(f"Participação no pseudo-R² de McFadden explicado pelos blocos "
              f"(total dos blocos: {br((sh.McFadden_completo.iloc[0] - sh.McFadden_ano.iloc[0]), 3)})")
limpa(ax)
ax.legend([Rectangle((0, 0), 1, 1, color=cores_b[b]) for b in sh.Bloco], [rot_b[b] for b in sh.Bloco],
          loc="lower center", bbox_to_anchor=(0.5, 1.0), ncol=3, frameon=False)
salva(fig, "N02_shapley_blocos.png")


ea = R["E1_descritivo_por_ano"]
ea = ea[ea.Ano_ingresso <= 2024]
fig, ax1 = plt.subplots(figsize=(W, 7.5 * CM))
ser = [("Taxa_exito", "Todos os cursos", TINTA, "o"), ("Exito_capacitacao", "Capacitação", AZUL, "s"),
       ("Exito_qualificacao", "Qualificação", LARANJA, "^"), ("Exito_tecnico_superior", "Técnico/Superior", CINZA, "D")]
for col, nome, cor, mk in ser:
    ax1.plot(ea.Ano_ingresso, ea[col] * 100, color=cor, marker=mk, ms=5, lw=1.6, label=nome)
    r = ea.iloc[-1]
    ax1.text(r.Ano_ingresso + 0.08, r[col] * 100, br(r[col] * 100) + "%", va="center", fontsize=8.5, color=cor)
ax1.set_xticks(ea.Ano_ingresso, [f"{a}\n(N = {mil(n)})" for a, n in zip(ea.Ano_ingresso, ea.Matriculas)])
ax1.set_ylim(0, 100)
fmt_eixo(ax1, "y", pct=True)
ax1.set_ylabel("Taxa de êxito")
for s in ("top", "right"):
    ax1.spines[s].set_visible(False)
ax1.yaxis.grid(True, color=GRADE, lw=0.6)
ax1.set_xlim(ea.Ano_ingresso.min() - 0.3, ea.Ano_ingresso.max() + 0.5)
ax1.legend(loc="lower center", bbox_to_anchor=(0.5, 1.0), ncol=4, frameon=False, fontsize=9)
salva(fig, "N03_exito_por_ano.png")


h3 = R["E1_Ha3_efeito_renda"]
fig, ax = plt.subplots(figsize=(W, 4.5 * CM))
for i, (_, r) in enumerate(h3.iterrows()):
    y = 1 - i
    c = cor_sig(r)
    ax.plot([r.IC_inf * 100, r.IC_sup * 100], [y, y], color=c, lw=1.8)
    ax.scatter(r.AME * 100, y, s=34, color=c, edgecolor="white", zorder=3)
    ax.text(r.IC_sup * 100 + 0.1, y, br(r.AME * 100, 2) + est(r.p), va="center", fontsize=9)
ax.axvline(0, color=TINTA2, lw=0.8)
ax.set_yticks([1, 0], ["Ocupados no ingresso", "Não ocupados no ingresso"])
ax.set_ylim(-0.6, 1.6)
fmt_eixo(ax, d=1)
ax.set_xlabel("Efeito de +1 faixa de renda sobre a probabilidade de êxito (pontos percentuais, IC 95%)")
limpa(ax)
salva(fig, "N04_renda_por_ocupacao.png")


ex = C["Exito_por_categoria"]
geral = R["E1_H1a_taxa_exito"].Taxa.iloc[0] * 100


def pontos_exito(ax, var, fs=8.5, rotulos=True):
    sub = ex[ex.Variavel == var].set_index("Categoria")
    cats = [c for c in ORDEM_CAT[var] if c in sub.index]
    sub = sub.reindex(cats)
    y = np.arange(len(cats))[::-1]
    for yi, (c, r) in zip(y, sub.iterrows()):
        cor = AZUL if r.IC_inf * 100 > geral else (LARANJA if r.IC_sup * 100 < geral else CINZA)
        ax.plot([r.IC_inf * 100, r.IC_sup * 100], [yi, yi], color=cor, lw=1.6)
        ax.scatter(r.Taxa_exito * 100, yi, s=24, color=cor, edgecolor="white", zorder=3)
        if rotulos:
            ax.text(r.IC_sup * 100 + 0.6, yi, f"{br(r.Taxa_exito * 100)}%  (N = {mil(r.N)})", va="center",
                    fontsize=fs - 1, color=TINTA2)
    ax.axvline(geral, color=TINTA2, lw=0.9, ls=(0, (4, 3)))
    ax.set_yticks(y, cats, fontsize=fs)
    ax.set_ylim(-0.6, len(cats) - 0.4)
    fmt_eixo(ax, pct=True)
    ax.tick_params(axis="x", labelsize=fs - 0.5)
    limpa(ax)


xmin = max(0, ex.IC_inf.min() * 100 - 5)
xmax = min(100, ex.IC_sup.max() * 100 + 5)
for var in ORDEM_CAT:
    if var not in ex.Variavel.values:
        continue
    n_cat = len(ORDEM_CAT[var])
    fig, ax = plt.subplots(figsize=(W, (n_cat * 0.9 + 1.8) * CM))
    pontos_exito(ax, var, fs=9.5)
    ax.set_xlim(xmin, xmax + 12)
    ax.set_xlabel(f"Taxa de êxito (IC 95%); linha tracejada: média geral ({br(geral)}%)")
    salva(fig, f"E_{ARQ[var]}.png", "exito_por_categoria")

vars_e = [v for v in ORDEM_CAT if v in ex.Variavel.values]
alt_e = [len(ORDEM_CAT[v]) + 1.2 for v in vars_e]
lin = [max(alt_e[i], alt_e[i + 1] if i + 1 < len(alt_e) else 0) for i in range(0, len(vars_e), 2)]
fig, axs = plt.subplots(len(lin), 2, figsize=(W, sum(lin) * 0.85 * CM + 2 * CM),
                        gridspec_kw={"height_ratios": lin, "hspace": 0.55, "wspace": 0.8})
for ax, var in zip(axs.flat, vars_e):
    pontos_exito(ax, var, fs=7.5, rotulos=False)
    ax.set_title(var, loc="left", fontsize=9.5, fontweight="bold")
    ax.set_xlim(xmin, xmax)
for ax in list(axs.flat)[len(vars_e):]:
    ax.axis("off")
fig.legend([Line2D([], [], color=AZUL, marker="o", ms=5), Line2D([], [], color=LARANJA, marker="o", ms=5),
            Line2D([], [], color=CINZA, marker="o", ms=5), Line2D([], [], color=TINTA2, ls=(0, (4, 3)))],
           ["Acima da média geral", "Abaixo da média geral", "Não difere da média", f"Média geral ({br(geral)}%)"],
           loc="lower center", ncol=4, frameon=False, bbox_to_anchor=(0.5, -0.01), fontsize=8.5)
salva(fig, "E_painel_exito_por_categoria.png", "exito_por_categoria")


# ==============================================================================

ps = C["P_sexo_EM"]
rot_em = {"EM1": "Subir de faixa de renda (EM1)", "EM2": "Conseguir ocupação, entre\nquem não trabalhava (EM2)",
          "EM3": "Ter vínculo formal na saída, entre\nquem não era formal (EM3)", "EM4": "Qualquer melhora (EM4)"}
fig, ax = plt.subplots(figsize=(W, 8 * CM))
for i, m in enumerate(["EM1", "EM2", "EM3", "EM4"]):
    y = 3 - i
    for val, off, cor, cl, mk in ((1, 0.13, LARANJA, LAR_C, "o"), (0, -0.13, AZUL, AZUL_C, "s")):
        r = ps[(ps.Modelo == m) & (ps.Valor == val)].iloc[0]
        ax.plot([r.IC_inf * 100, r.IC_sup * 100], [y + off] * 2, color=cl, lw=1.8, zorder=1)
        ax.scatter(r.Prob * 100, y + off, s=40, marker=mk, color=cor, edgecolor="white", lw=0.8, zorder=3)
        ax.text(r.IC_sup * 100 + 1, y + off, br(r.Prob * 100) + "%", va="center", fontsize=9)
ax.set_yticks([3, 2, 1, 0], [rot_em[m] for m in ["EM1", "EM2", "EM3", "EM4"]])
ax.set_xlim(0, max(ps.IC_sup.max() * 100 + 8, 70))
ax.set_ylim(-0.5, 3.6)
fmt_eixo(ax, pct=True)
ax.set_xlabel("Probabilidade prevista (IC 95%)")
limpa(ax)
ax.legend([Line2D([], [], marker="o", ls="", color=LARANJA, ms=6), Line2D([], [], marker="s", ls="", color=AZUL, ms=6)],
          ["Mulheres", "Homens"], loc="lower center", bbox_to_anchor=(0.5, 1.0), ncol=2, frameon=False)
salva(fig, "G04_prob_prevista_sexo.png")


ame2 = R["E2_AME"]
paineis = [("EM1", "EM1\nSubir de faixa\nde renda"), ("EM2", "EM2\nConseguir ocupação\n(não ocupados)"),
           ("EM3", "EM3\nVínculo formal\n(não formais)"), ("EM4", "EM4\nQualquer\nmelhora")]
base = prepara_ame(ame2[ame2.Modelo.isin([p for p, _ in paineis])])
itens = list(dict.fromkeys(zip(base.grupo, base.item)))
n = len(itens)
fig, axs = plt.subplots(1, 4, figsize=(W, (n * 0.62 + 3) * CM), sharey=True, gridspec_kw={"wspace": 0.08})
lo, hi = base.IC_inf.min(), base.IC_sup.max()
for ax, (m, tit) in zip(axs, paineis):
    sub = base[base.Modelo == m]
    for k, (g, it) in enumerate(itens):
        y = n - 1 - k
        r = sub[(sub.grupo == g) & (sub["item"] == it)]
        if r.empty:
            ax.plot([-3, 3], [y, y], color=CINZA_C, lw=0.8)
            continue
        r = r.iloc[0]
        c = cor_sig(r)
        ax.plot([r.IC_inf, r.IC_sup], [y, y], color=c, lw=1.6, solid_capstyle="round")
        ax.scatter(r.AME, y, s=22, color=c, edgecolor="white", lw=0.7, zorder=3)
    ax.axvline(0, color=TINTA2, lw=0.8)
    ax.set_xlim(lo - 3, hi + 3)
    ax.set_title(tit, fontsize=9.5)
    limpa(ax)
    fmt_eixo(ax)
    ax.tick_params(axis="x", labelsize=8.5)
lab = []
for g, it in itens:
    lab.append(it if g in ("Perfil", "Socio", "Transição") else f"{dict(CURTO).get(g, g)}: {it}")
axs[0].set_yticks(range(n - 1, -1, -1), lab, fontsize=9)
axs[0].set_ylim(-0.6, n - 0.4)
fig.supxlabel("Efeito marginal médio (pontos percentuais, IC 95%)", fontsize=10, y=0.0)
salva(fig, "G05_ame_em1_em4.png")


pe = C["P_EM1a_EM1b"]
fig, ax = plt.subplots(figsize=(W, 8.5 * CM))
xs = [0, 1, 2.8, 3.8]
for x0, (_, r) in zip(xs, pe.iterrows()):
    cor = AZUL if r.Valor == 1 else CINZA
    ax.bar(x0, r.Prob * 100, width=0.8, color=cor, zorder=2)
    ax.plot([x0, x0], [r.IC_inf * 100, r.IC_sup * 100], color=TINTA2, lw=1.2, zorder=3)
    ax.text(x0, r.IC_sup * 100 + 1.5, br(r.Prob * 100) + "%", ha="center", va="bottom", fontsize=9.5)
ax.set_xticks(xs, ["Não", "Sim", "Não", "Sim"])
for xm, t in ((0.5, "Conseguiu ocupação\n(EM1a)"), (3.3, "Passou a ter vínculo formal\n(EM1b)")):
    ax.text(xm, -0.17, t, transform=ax.get_xaxis_transform(), ha="center", va="top", fontsize=10)
ax.set_ylim(0, max(pe.IC_sup.max() * 100 + 10, 70))
fmt_eixo(ax, "y", pct=True)
ax.set_ylabel("Probabilidade prevista de\nsubir de faixa de renda")
for s in ("top", "right"):
    ax.spines[s].set_visible(False)
ax.yaxis.grid(True, color=GRADE, lw=0.6)
ax.set_axisbelow(True)
ax.tick_params(axis="x", length=0)
salva(fig, "G06_prob_prevista_em1a_em1b.png")


# ==============================================================================


d = C["Cor_nao_informada_ano"]
anos = d[d.Ano.notna()]
media = d[d.Ano.isna()].Nao_informada.iloc[0] * 100
fig, ax = plt.subplots(figsize=(W, 7 * CM))
x = anos.Ano.astype(int).astype(str)
v = anos.Nao_informada * 100
bars = ax.bar(x, v, width=0.55, color=AZUL, zorder=2)
for b, val in zip(bars, v):
    ax.text(b.get_x() + b.get_width() / 2, val + 1.5, br(val) + "%", ha="center", va="bottom", color=TINTA)
ax.axhline(media, color=TINTA2, lw=0.9, ls=(0, (4, 3)))
ax.text(-0.42, media + 1.4, f"Média dos egressos: {br(media)}%", color=TINTA2, fontsize=9, va="bottom")
ax.set_ylim(0, max(v.max() * 1.18, 65))
ax.set_ylabel("Cor/raça “não informada” (%)")
for s in ("top", "right"):
    ax.spines[s].set_visible(False)
ax.yaxis.grid(True, color=GRADE, lw=0.6)
ax.set_axisbelow(True)
ax.tick_params(axis="x", length=0)
salva(fig, "G01_cor_nao_informada_ano.png")



def pessoa(ax, x, y, cor, contorno=False):
    kw = dict(facecolor="white" if contorno else cor, edgecolor=cor, lw=1.3 if contorno else 0)
    ax.add_patch(Circle((x, y + 0.22), 0.11, **kw))
    ax.add_patch(FancyBboxPatch((x - 0.13, y - 0.32), 0.26, 0.38, boxstyle="round,pad=0,rounding_size=0.07", **kw))


def grade_pessoas(ax, cores, contorno=()):
    for i, c in enumerate(cores):
        r, col = divmod(i, 10)
        pessoa(ax, col, 9 - r, c, contorno=i in contorno)
    ax.set_xlim(-0.6, 9.6)
    ax.set_ylim(-0.7, 9.7)
    ax.set_aspect("equal")
    ax.axis("off")



ipm = R["E3_IPM"]
ipm = ipm[ipm.Indicador.str.startswith("MS")]
fig, axs = plt.subplots(1, 3, figsize=(W, 7.6 * CM), gridspec_kw={"wspace": 0.12})
for ax, (_, r) in zip(axs, ipm.iterrows()):
    k = int(round(r.Proporcao * 100))
    cores = [CINZA_C] * 100
    for j in range(k):
        lin, col = divmod(j, 10)
        cores[(9 - lin) * 10 + col] = AZUL
    grade_pessoas(ax, cores)
    ax.set_title(r.Base, fontsize=11, pad=4)
    ax.text(4.5, -1.5, f"{br(r.Proporcao * 100)}% melhoraram\nIPM ≈ {br(r.IPM, 2)}  (N = {mil(r.N)})",
            ha="center", va="top", fontsize=9.5, linespacing=1.4)
salva(fig, "G07_ipm_pictograma.png")

att = R["E3_ATT_CEM"]
g = att[att.Especificacao == "GO sem cor (principal) | Geral"].set_index("Desfecho")
oc, fo = g.loc["Ocupação"], g.loc["Formalização"]
fig = plt.figure(figsize=(W, W * 1.42))


def bloco(y0, titulo, egr, pnad, n, rot, modo, a, p):
    fig.text(0.07, y0, titulo, fontsize=11.5, fontweight="bold", va="top")
    axe = fig.add_axes([0.06, y0 - 0.335, 0.42, 0.27])
    axp = fig.add_axes([0.53, y0 - 0.335, 0.42, 0.27])
    e, q = int(round(egr * 100)), int(round(pnad * 100))
    if e < q:
        ce = [AZUL] * e + [LARANJA] * (q - e) + [CINZA_C] * (100 - q)
        grade_pessoas(axe, ce, contorno=set(range(e, q)))
        cor_txt, sinal, ind = LARANJA, "−", "NNH"
    else:
        ce = [AZUL] * q + [VERDE] * (e - q) + [CINZA_C] * (100 - e)
        grade_pessoas(axe, ce)
        cor_txt, sinal, ind = VERDE_T, "+", "NNT"
    grade_pessoas(axp, [AZUL] * q + [CINZA_C] * (100 - q))
    axe.set_title("Egressos", fontsize=10.5)
    axp.set_title("População comparável (PNAD)", fontsize=10.5)
    fig.text(0.27, y0 - 0.355, f"{br(egr * 100)}% {rot} (N = {mil(n)})", ha="center", va="top", fontsize=9.5)
    fig.text(0.74, y0 - 0.355, f"{br(pnad * 100)}% {rot}", ha="center", va="top", fontsize=9.5)
    final = f"{ind} ≈ {br(1 / abs(a), 2)}" if p < 0.05 else "diferença não significativa"
    fig.text(0.5, y0 - 0.385, f"Diferença: {sinal}{br(abs(a) * 100)} p.p.   →   {final}",
             ha="center", va="top", fontsize=10, fontweight="bold", color=cor_txt if p < 0.05 else TINTA2)


bloco(0.985, "Ocupação (entre os que estão na força de trabalho)", oc.Media_tratados, oc.Media_controles,
      oc.N_tratados_desfecho, "ocupados", "ocup", oc.ATT, oc.p)
bloco(0.54, "Formalização (entre os ocupados)", fo.Media_tratados, fo.Media_controles,
      fo.N_tratados_desfecho, "formais", "form", fo.ATT, fo.p)
leg = [(Rectangle((0, 0), 1, 1, fc=AZUL), "Ocupado / formal"),
       (Rectangle((0, 0), 1, 1, fc="white", ec=LARANJA, lw=1.5), "Ocupado na PNAD, mas não entre egressos"),
       (Rectangle((0, 0), 1, 1, fc=VERDE), "Formal entre egressos, a mais que na PNAD"),
       (Rectangle((0, 0), 1, 1, fc=CINZA_C), "Demais")]
fig.legend([h for h, _ in leg], [t for _, t in leg], loc="lower center", ncol=2, frameon=False, fontsize=8.5,
           bbox_to_anchor=(0.5, 0.005), handlelength=1.0, columnspacing=1.5)
salva(fig, "G08_egressos_pnad.png")


BASES = ["Geral", "Capacitação", "Qualificação"]
PAIN = [("Ocupação", "Ocupação: efeito sobre a probabilidade de estar ocupado (pontos de proporção)", 0.02),
        ("Formalização", "Formalização: efeito sobre a probabilidade de vínculo formal (pontos de proporção)", 0.05),
        ("Renda (faixa)", "Renda: efeito sobre a faixa de renda (escala ordinal 0 a 5)", 0.1),
        ("Renda (faixa, ocupados)", "Renda entre os ocupados: efeito sobre a faixa de renda (escala 0 a 5)", 0.1)]


def linha_att(spec, base, desf):
    return att[(att.Especificacao == f"{spec} | {base}") & (att.Desfecho == desf)].iloc[0]


fig, axs = plt.subplots(4, 1, figsize=(W, 14 * CM), gridspec_kw={"hspace": 1.0})
for ax, (desf, tit, passo) in zip(axs, PAIN):
    for i, b in enumerate(BASES):
        y = 2 - i
        s, c = linha_att("GO sem cor (principal)", b, desf), linha_att("GO com cor", b, desf)
        ys, yc = y + 0.13, y - 0.13
        ax.plot([s.IC_inf, s.IC_sup], [ys, ys], color=AZUL_C, lw=1.5, zorder=1)
        ax.plot([c.IC_inf, c.IC_sup], [yc, yc], color=LAR_C, lw=1.5, zorder=1)
        ax.plot([s.ATT, c.ATT], [ys, yc], color="#9a9a96", lw=1.2, zorder=2)
        ax.scatter(s.ATT, ys, s=30, color=AZUL, edgecolor="white", lw=0.8, zorder=3)
        ax.scatter(c.ATT, yc, s=34, marker="^", color=LARANJA, edgecolor="white", lw=0.7, zorder=3)
    ax.axvline(0, color=TINTA2, lw=0.9)
    ax.set_yticks([2, 1, 0], BASES)
    ax.set_ylim(-0.5, 2.5)
    ax.set_title(tit, loc="left", fontsize=9.5, pad=3)
    limpa(ax)
    ax.xaxis.set_major_formatter(mtick.FuncFormatter(lambda v, _: br(v, 2)))
    ax.xaxis.set_major_locator(mtick.MultipleLocator(passo))
    lo_, hi_ = ax.get_xlim()
    ax.set_xlim(min(lo_, -0.01), max(hi_, 0.01))
fig.legend([Line2D([], [], marker="o", ls="", color=AZUL, ms=6), Line2D([], [], marker="^", ls="", color=LARANJA, ms=6)],
           ["Sem cor/raça no pareamento (principal)", "Com cor/raça no pareamento (robustez)"],
           loc="lower center", ncol=2, frameon=False, bbox_to_anchor=(0.5, -0.04))
salva(fig, "G09_att_com_sem_cor.png")


lee = R["E3_Lee"]
fig, axs = plt.subplots(1, 2, figsize=(W, 6 * CM), gridspec_kw={"wspace": 0.35})
for ax, (desf, tit, k, unid) in zip(axs, [("Formalização", "Formalização", 100, "pontos percentuais"),
                                         ("Renda (faixa, ocupados)", "Renda entre os ocupados", 1, "faixas de renda")]):
    for i, b in enumerate(BASES):
        y = 2 - i
        r = linha_att("GO sem cor (principal)", b, desf)
        l = lee[(lee.Especificacao == f"GO sem cor (principal) | {b}") & (lee.Desfecho == desf)].iloc[0]
        ax.add_patch(Rectangle((l.Limite_inferior * k, y - 0.22), (l.Limite_superior - l.Limite_inferior) * k, 0.44,
                               color=AZUL_C, alpha=0.6, lw=0, zorder=1))
        c = AZUL if (r.p < 0.05 and r.ATT > 0) else (LARANJA if r.p < 0.05 else CINZA)
        ax.plot([r.IC_inf * k, r.IC_sup * k], [y, y], color=c, lw=1.6, zorder=2)
        ax.scatter(r.ATT * k, y, s=30, color=c, edgecolor="white", zorder=3)
    ax.axvline(0, color=TINTA2, lw=0.8)
    ax.set_yticks([2, 1, 0], BASES)
    ax.set_ylim(-0.6, 2.6)
    ax.set_title(tit, fontsize=10)
    ax.set_xlabel(f"ATT ({unid})", fontsize=9)
    fmt_eixo(ax, d=0 if k == 100 else 1)
    limpa(ax)
fig.legend([Line2D([], [], marker="o", color=AZUL, ms=5, lw=1.5), Rectangle((0, 0), 1, 1, color=AZUL_C, alpha=0.6)],
           ["ATT e IC 95%", "Limites de Lee (2009)"], loc="upper center", ncol=2, frameon=False, bbox_to_anchor=(0.5, -0.02))
salva(fig, "N05_att_limites_lee.png")


SPECS = [("GO sem cor (principal)", "Goiás (principal)"), ("GO com cor", "Goiás, com cor/raça"),
         ("GO pesos V1028", "Goiás, com pesos da PNAD"), ("CO sem cor", "Centro-Oeste"), ("BR sem cor", "Brasil")]
fig, axs = plt.subplots(1, 4, figsize=(W, 7 * CM), sharey=True, gridspec_kw={"wspace": 0.12})
for ax, (desf, tit, k) in zip(axs, [("Ocupação", "Ocupação\n(p.p.)", 100), ("Formalização", "Formalização\n(p.p.)", 100),
                                    ("Renda (faixa)", "Renda\n(faixas)", 1), ("Renda (faixa, ocupados)", "Renda, ocupados\n(faixas)", 1)]):
    for i, (s, _) in enumerate(SPECS):
        y = len(SPECS) - 1 - i
        r = linha_att(s, "Geral", desf)
        c = CINZA if r.p >= 0.05 else (AZUL if r.ATT > 0 else LARANJA)
        ax.plot([r.IC_inf * k, r.IC_sup * k], [y, y], color=c, lw=1.6)
        ax.scatter(r.ATT * k, y, s=26, color=c, edgecolor="white", zorder=3, marker="o" if i == 0 else "D")
    ax.axvline(0, color=TINTA2, lw=0.8)
    ax.set_title(tit, fontsize=9.5)
    fmt_eixo(ax, d=0 if k == 100 else 1)
    ax.tick_params(axis="x", labelsize=8.5)
    limpa(ax)
axs[0].set_yticks(range(len(SPECS) - 1, -1, -1), [t for _, t in SPECS])
axs[0].set_ylim(-0.6, len(SPECS) - 0.4)
salva(fig, "N06_att_especificacoes.png")



# ==============================================================================


dist = C["Dist_categorias"]
grupos = [g for g in GRUPO_COR if g in dist.Grupo.unique()]


def barras_dist(ax, var, rotulos=True, fs=8):
    sub = dist[dist.Variavel == var]
    cats = [c for c in ORDEM_CAT[var] if c in sub.Categoria.values]
    gs = [g for g in grupos if g in sub.Grupo.values]
    h = 0.8 / len(gs)
    for j, g in enumerate(gs):
        s = sub[sub.Grupo == g].set_index("Categoria").reindex(cats)
        y = np.arange(len(cats))[::-1] + 0.4 - h * (j + 0.5)
        ax.barh(y, s.Proporcao * 100, height=h * 0.92, color=GRUPO_COR[g], zorder=2, label=g)
        if rotulos:
            for yi, val in zip(y, s.Proporcao * 100):
                if not np.isnan(val):
                    ax.text(val + 0.8, yi, br(val), va="center", fontsize=fs - 1, color=TINTA2)
    ax.set_yticks(np.arange(len(cats))[::-1], cats, fontsize=fs + 0.5)
    ax.set_xlim(0, sub.Proporcao.max() * 100 * 1.18)
    fmt_eixo(ax, pct=True)
    ax.tick_params(axis="x", labelsize=fs)
    limpa(ax)


def legenda_grupos(fig, y=-0.02):
    fig.legend([Rectangle((0, 0), 1, 1, color=GRUPO_COR[g]) for g in grupos], grupos, loc="lower center",
               ncol=len(grupos), frameon=False, bbox_to_anchor=(0.5, y), fontsize=9)


for var in ORDEM_CAT:
    n_cat = len(ORDEM_CAT[var])
    fig, ax = plt.subplots(figsize=(W, (n_cat * 1.15 + 2) * CM))
    barras_dist(ax, var, fs=9)
    ax.set_xlabel("Percentual do grupo")
    legenda_grupos(fig, -0.06 if n_cat <= 3 else -0.03)
    salva(fig, f"D_{ARQ[var]}.png", "demografia")

vars_p = list(ORDEM_CAT)
alturas = [len(ORDEM_CAT[v]) + 1.2 for v in vars_p]
lin = [max(alturas[i], alturas[i + 1]) for i in range(0, len(vars_p), 2)]
fig, axs = plt.subplots(len(lin), 2, figsize=(W, sum(lin) * 0.95 * CM + 2 * CM),
                        gridspec_kw={"height_ratios": lin, "hspace": 0.55, "wspace": 0.75})
for ax, var in zip(axs.flat, vars_p):
    barras_dist(ax, var, rotulos=False, fs=7.5)
    ax.set_title(var, loc="left", fontsize=9.5, fontweight="bold")
legenda_grupos(fig, -0.01)
salva(fig, "D_painel_demografia.png", "demografia")



def aluvial(ax, tab, cats, cores, cor_fluxo=None):
    tab = tab.reindex(index=cats, columns=cats, fill_value=0)
    n = tab.values.sum()
    gap = 0.018
    alt = lambda c: c / n * (1 - gap * (len(cats) - 1))

    def blocos(tot):
        y, pos = 1.0, {}
        for c in cats:
            h = alt(tot[c])
            pos[c] = [y - h, y]
            y -= h + gap
        return pos

    pe, pdir = blocos(tab.sum(axis=1)), blocos(tab.sum(axis=0))
    xl, xr, bw = 0.0, 1.0, 0.06
    te = {c: pe[c][1] for c in cats}
    td = {c: pdir[c][1] for c in cats}
    for ci in cats:
        for cj in cats:
            f = tab.loc[ci, cj]
            if f == 0:
                continue
            h = alt(f)
            y0e, y0d = te[ci], td[cj]
            te[ci] -= h
            td[cj] -= h
            xa, xb = xl + bw, xr - bw
            xm = (xa + xb) / 2
            verts = [(xa, y0e), (xm, y0e), (xm, y0d), (xb, y0d), (xb, y0d - h), (xm, y0d - h),
                     (xm, y0e - h), (xa, y0e - h), (xa, y0e)]
            codes = [Path.MOVETO, Path.CURVE4, Path.CURVE4, Path.CURVE4, Path.LINETO,
                     Path.CURVE4, Path.CURVE4, Path.CURVE4, Path.CLOSEPOLY]
            fc, al = cor_fluxo(ci, cj) if cor_fluxo else (cores[ci], 0.42)
            ax.add_patch(PathPatch(Path(verts, codes), facecolor=fc, alpha=al, lw=0))
    for pos, x0, lado in ((pe, xl, "e"), (pdir, xr - bw, "d")):
        tot = tab.sum(axis=1) if lado == "e" else tab.sum(axis=0)
        for c in cats:
            y0, y1 = pos[c]
            ax.add_patch(Rectangle((x0, y0), bw, y1 - y0, color=cores[c], lw=0))
            if y1 - y0 > 0.012:
                t = f"{c} ({br(tot[c] / n * 100)}%)"
                if lado == "e":
                    ax.text(x0 - 0.02, (y0 + y1) / 2, t, ha="right", va="center", fontsize=8.5)
                else:
                    ax.text(x0 + bw + 0.02, (y0 + y1) / 2, t, ha="left", va="center", fontsize=8.5)
    ax.text(xl + bw / 2, 1.04, "No ingresso", ha="center", va="bottom", fontsize=9.5, color=TINTA2)
    ax.text(xr - bw / 2, 1.04, "Na pesquisa com egressos", ha="center", va="bottom", fontsize=9.5, color=TINTA2)
    ax.text(0.5, -0.06, f"N = {mil(n)}", ha="center", va="top", fontsize=9, color=TINTA2)
    ax.set_xlim(-0.55, 1.55)
    ax.set_ylim(-0.1, 1.1)
    ax.axis("off")


ao = C["Aluvial_ocupacao"].pivot_table(index="Ingresso", columns="Egresso", values="Freq", aggfunc="sum", fill_value=0)
cats_o = ["Ocupado", "Desocupado", "Inativo"]
fig, ax = plt.subplots(figsize=(W, 9 * CM))
aluvial(ax, ao, cats_o, {"Ocupado": AZUL, "Desocupado": LARANJA, "Inativo": VERDE})
salva(fig, "A01_aluvial_ocupacao.png")

ar = C["Aluvial_renda"].pivot_table(index="Ingresso", columns="Egresso", values="Freq", aggfunc="sum", fill_value=0)
cats_r = ["Sem renda", "Até 1 SM", "Mais de 1 a 2 SM", "Mais de 2 a 5 SM", "Mais de 5 SM"]
cores_r = {"Sem renda": "#b7d3f6", "Até 1 SM": "#86b6ef", "Mais de 1 a 2 SM": "#5598e7",
           "Mais de 2 a 5 SM": "#2a78d6", "Mais de 5 SM": "#184f95"}
nivel = {c: i for i, c in enumerate(cats_r)}
fig, ax = plt.subplots(figsize=(W, 10 * CM))
aluvial(ax, ar, cats_r, cores_r, lambda a, b: (AZUL, 0.5) if nivel[b] > nivel[a]
        else ((LARANJA, 0.5) if nivel[b] < nivel[a] else (CINZA, 0.35)))
ax.legend([Rectangle((0, 0), 1, 1, color=AZUL, alpha=0.6), Rectangle((0, 0), 1, 1, color=CINZA, alpha=0.5),
           Rectangle((0, 0), 1, 1, color=LARANJA, alpha=0.6)], ["Subiu de faixa", "Manteve a faixa", "Desceu de faixa"],
          loc="upper center", bbox_to_anchor=(0.5, -0.02), ncol=3, frameon=False, fontsize=8.5)
salva(fig, "A02_aluvial_renda.png")

print("\nGráficos salvos em:", OUT)

