# 🗳️ Sentiment and Image Analysis in Political Approval Ratings

A Social Media Data Analytics Approach

This repository contains an R-based multiplatform sentiment analysis project that examines public opinion toward the U.S. President and four congressional leaders using data from Twitter, Reddit, and online news comment sections. The analysis spans Q1 2025 and includes sentiment classification, event-driven trends, geographic/demographic breakdowns, and comparison with traditional polling.

---

## 📂 Project Structure

| Folder / File | Description |
|---------------|-------------|
| `data/raw/` | Original raw datasets from Twitter, Reddit, and news comments |
| `data/processed/` | Cleaned datasets after tokenization, filtering, and sentiment labeling |
| `plots/` | Generated visualizations including sentiment trends, CLV maps, etc. |
| `Report.docx` | Full documentation of research methods, models, results, and references |
| `README.md` | Overview, objectives, setup, and structure of the repository |



---

## 🎯 Research Objectives

- Analyze fluctuations in political sentiment in response to major events  
- Identify platform-specific sentiment expression differences  
- Understand geographic and demographic sentiment variations  
- Evaluate correlation between social media sentiment and polling data  

---

## 🔍 Methodology Overview

**Data Sources**:
- Twitter API v2 (via `rtweet`)
- Reddit subreddits (via `RedditExtractoR`)
- News comment scraping (via `rvest`)

**Preprocessing**:
- Tokenization, normalization, lemmatization (`textstem`)
- Stopword removal (`stopwords`)
- TF-IDF matrix construction

**Sentiment Techniques**:
- Lexicon-based (AFINN, Bing, NRC via `tidytext`)
- Machine Learning (Random Forest via `randomForest`)
- Topic Modeling (LDA via `topicmodels`)

**Analysis Types**:
- Temporal: Daily/Weekly, Event-based (`zoo`)
- Geographic: State-level maps (`maps`)
- Demographic: Platform-based proxies
- Polling Correlation: Trump sentiment vs. approval rating (r = 0.65)

---

## 📊 Key Visual Insights

- 📉 Drop in Trump sentiment after March tariff announcement
- 🌍 Georgia showed strongest negative sentiment regionally
- 📱 Twitter (younger, urban) more critical than Reddit (neutral)
- 📈 Sentiment trends aligned with simulated polling dips

---

## 💡 Implications & Recommendations

- Real-time social sentiment tracking can complement traditional polling
- Multiplatform approaches reduce single-platform bias
- Useful for campaign strategy, media framing, and policy feedback loops

---

## ⚠️ Limitations

- Sparse geographic metadata (10% of posts)
- Lack of Twitter data for some political figures
- Placeholder data used for news comments and polls

---

## 🧠 Future Work

- Incorporate real news APIs and polling datasets
- Collect survey-based demographic metadata
- Use transformer-based sentiment models (e.g., BERT)

---

## 📚 References

Refer to the full report (`Report.docx`) for all academic citations.

---

