# 📊 Academic Performance Analysis using Principal Component Analysis

A statistical analysis of student academic performance using Principal Component Analysis (PCA). This project examines deliberation records from École nationale Supérieure d'Informatique (ESI) to reveal patterns in performance and their relationship with specialization assignments.

## 📚 Table of Contents
- Overview
- Objectives
- Dataset
- Specializations
- Methodology
- Key Findings
- File Structure
- Technologies
- Key Visualizations
- Installation & Usage
- Results Interpretation
- Authors & Supervision
- License
- Conclusion

## 📝 Overview
This study applies PCA to academic records (2021–2022) to reduce dimensionality, visualize relationships between modules, and characterize student profiles by specialization.

## 🎯 Objectives
- Analyze correlations between course modules and student results
- Identify performance indicators influencing specialization placement
- Reduce dimensionality while preserving key information
- Characterize profiles for SID, SIQ, SIL and SIT specializations

## 📂 Dataset
Covers three academic levels:
- **1CP (2021–2022)** — First year preparatory class
- **2CP (2021–2022)** — Second year preparatory class
- **1CS (2021–2022)** — First year second cycle

### 🧑‍💻 Specializations
- **SID** — Systèmes Intelligents et Données (Intelligent Systems & Data)  
- **SIQ** — Systèmes Informatiques (Computer Systems)  
- **SIL** — Systèmes Informatiques et Logiciels (Systems & Software)  
- **SIT** — Systèmes d'Information et Technologies (Information Systems & Technologies)

## ⚙️ Methodology
1. Data preprocessing
    - Merge deliberation records with specialization assignments
    - Remove irrelevant columns and outliers
    - Handle missing values and inconsistencies
2. Correlation analysis
    - Compute correlation matrices to find inter-module relationships
3. Principal Component Analysis (PCA)
    - Normalize data, compute eigenvalues and explained variance
    - Interpret principal components and variable contributions
4. Individual & variable analysis
    - Study contributions of individuals to axes
    - Assess quality of representation on factorial planes
    - Characterize profiles by specialization

## 🔑 Key Findings
- First factorial plane captures ~76.11% of variance in the initial analysis
- Strong correlations between technical modules (e.g., RES2–ARCH, SYS1–SYS2)
- Distinct performance patterns across specializations
- Academic performance is informative but not the sole determinant of specialization choice

## 💻 Technologies
- R
- Key packages: FactoMineR, factoextra, corrplot, ggplot2, readxl, dplyr

## 📈 Key Visualizations
- Correlation matrices (inter-module relationships)
- Scree plots (eigenvalue analysis)
- Biplots of individuals and variables
- Contribution plots for axis interpretation
- Quality of representation analyses

## 🧐 Results Interpretation
- **Axis 1** — Performance measure: captures general technical performance (systems, networks, databases, architecture).
- **Axis 2** — Competency opposition: contrasts theoretical/mathematical strengths with project-based and communication skills.

Specialization characteristics:
- **SID** — Balanced; strength in data-oriented modules
- **SIQ** — Strong theoretical foundations (programming theory)
- **SIL** — Average across most modules
- **SIT** — Stronger in soft skills and project management

## 👥 Authors
- ABOUD Ibrahim  
- BOUYAKOUB Rayane

Institution: École nationale Supérieure d'Informatique (ESI)  
Program: 2CS - Systèmes Intelligents et Données (SID)  
Academic Year: 2024–2025

## 🧑‍🏫 Supervision
- Supervisor: Mme HAMDAD Leila

## 📄 License
Developed for academic purposes as part of the ANAD (Analyse et fouille de données) course.

## 🏁 Conclusion
PCA provides a compact representation of academic performance and highlights meaningful patterns tied to specializations. While grades yield valuable insights, specialization choice depends on additional factors beyond academic scores.