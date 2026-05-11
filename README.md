
# Evaluating the Statistical Reasoning of LLM in Missing Data Imputation

This project investigates the reliability and statistical reasoning capabilities of LLM-based data agents, focusing on their ability to recognize and challenge incorrect statistical assumptions.

---

## 📂 Project Structure & Navigation
Based on our current research progress, the repository is organized as follows:

* **Research Progress (`thesis/`)**: Contains the *Research Direction Proposal*. Future thesis chapters and draft sections will be updated here as the empirical study progresses.
* **References & Literature (`literature/`)**: Includes seed references and foundational readings.
* **Empirical Analysis (`analysis/`)**: Houses the experimental design for evaluating agents under four prompt conditions: **Blind**, **Reasoning**, **Direct instruction**, and **Misleading**.
* **Presentations (`presentation/`)**: Contains slides and materials for project reviews and academic discussions.

---

## 🎯 Research Focus: Missing Data Imputation (MNAR)
The core of this study is a small empirical investigation into how AI agents handle **Missing Not At Random (MNAR)** data mechanisms. We aim to determine:

* Whether the agent **passively accepts** user-provided (and potentially incorrect) assumptions.
* If the agent can **infer or question** the missingness mechanism based on contextual reasoning.
* How the agent **adjusts its imputation strategy** when faced with misleading prompts.

---

## ⚙️ Methodology & Approach
> We evaluate trust in human-AI collaboration via the agent's ability to recognize incorrect assumptions.

1. **Dataset Construction**: Artificially inducing MNAR mechanisms into real-world datasets to create controlled testing environments.
2. **Comparative Prompting**: Testing the agent's statistical reasoning across different levels of guidance and misdirection.
3. **Evaluation**: A hybrid approach combining quantitative metrics (imputed value correctness) with qualitative analysis of the agent's code and reasoning process.

---

## 🛠️ How to Use

* **Literature Review**: Start by checking `literature/` for an overview of the foundational papers.
* **Tracking Progress**: Review the `thesis/` directory to see the latest updates on the Introduction, Methods, and Results.
* **Running Analysis**: If you have the required data, the analysis pipeline can be executed from the `analysis/` directory.