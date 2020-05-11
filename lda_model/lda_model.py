
pip install "wordcloud"

import os
import re
import io
import nltk
import collections 
from wordcloud import WordCloud
import numpy as np
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.decomposition import LatentDirichletAllocation as LDA
import glob

file_location = os.path.join('python_R', 'scripts',"scrapped", '*.txt')

print(file_location)


files = glob.glob(file_location)

abstracts = []

for i in files:
  lines = open(i,"r",errors='ignore')
  text = lines.read()
  abstracts.append(text)


# Wordcloud

abstract_df = pd.DataFrame({"abs" : abstracts})

long_s = ','.join(list(abstract_df["abs"].values))

word = WordCloud(background_color="white", max_words=10000, contour_width=3, contour_color='steelblue')

word = word.generate(long_s)  

word.to_file("alex.png")

# Tokenize

tokens = nltk.tokenize.word_tokenize(long_s)

count_vectorizer = CountVectorizer(stop_words='english')

cnt = collections.Counter(tokens)

# Plot top 10

lat = cnt.most_common(10)

labels, values = zip(*lat)

indexes = np.arange(len(labels))
width = 1

plt.clf()
plt.bar(indexes, values, width)
plt.xticks(indexes + width * 0.5, labels)
plt.show()

# Processing

count_data = count_vectorizer.fit_transform(tokens)


def print_topics(model, count_vectorizer, n_top_words):
    words = count_vectorizer.get_feature_names()
    for topic_idx, topic in enumerate(model.components_):
        print("\nTopic #%d:" % topic_idx)
        print(" ".join([words[i]
                        for i in topic.argsort()[:-n_top_words - 1:-1]]))
                        
                        
number_topics = 5
number_words = 10

# Create and fit the LDA model

lda = LDA(n_components=number_topics)
lda.fit(count_data)

# Print the topics found by the LDA model

print("Topics found via LDA:")
print_topics(lda, count_vectorizer, number_words)


