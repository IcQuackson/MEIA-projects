# 📘 Project 1 — Task Assignments

## **👥 Daniel & Henrique**
### **1. Exploratory Data Analysis (EDA)**
- Explorar e descrever os dados utilizando métodos estudados na UC.
- Produzir gráficos relevantes (histogramas, boxplots, pairs, correlogramas).
- Calcular estatísticas descritivas:
  - média, mediana
  - trimmed mean & winsorized mean
  - variância, MAD
  - covariância
  - generalized variance & total variance
  - distâncias de Mahalanobis
- Discussão final: o que se aprende com esta análise preliminar.

---

## **👤 Pedro**
### **2(a). PCA — Escala original**
- Aplicar PCA utilizando:
  - variáveis na **escala original**
  - matriz de covariância clássica
- Produzir scree plot, variância explicada, loadings e scores.

---

## **👤 Maria**
### **2(a ii). PCA — Variáveis Padronizadas**
- Aplicar PCA utilizando:
  - variáveis **standardized (scale = TRUE)**
- Produzir scree plot, variância explicada, loadings e scores.

---

## **👥 Pedro & Maria**
### **3. Comparação das abordagens de PCA & Redução de Dimensionalidade**
- Determinar qual das duas análises (escala original vs standard) é recomendada.
- Critérios:
  - manter pelo menos **95% da variância total**.
  - comparar percentagens de variância explicada dos PCs.
- Interpretar os PCs retidos.
- Criar plot usando os scores das PCs escolhidas.
- Redigir a conclusão técnica.

---

## **👤 Gui**
### **4(a). Outlier — PCA Clássico**
- Alterar observação `hp-3000/64` para: (75, 2000, 0.8, 80000, 300, 24, 62, 47)
- Aplicar PCA clássico **sem standardization** ao novo dataset.
- Comparar alterações face à análise original:
- variância explicada
- loadings
- scores
- detetar distorções provocadas pelo outlier

---

## **👤 Daniela**
### **4(b). Outlier — PCA Robusto (MCD)**
- Aplicar PCA robusto baseado no **MCD estimate**.
- Comparar resultados com o PCA clássico.
- Discutir impacto do ponto atípico:
- estabilidade dos loadings
- PCs afetados / não afetados
- diferenças nos scores
- sensibilidade vs robustez

---

# ✅ Recomendações finais
- Cada membro deve incluir gráficos + interpretação.
- Consolidar tudo no relatório final após cada parte estar pronta.
