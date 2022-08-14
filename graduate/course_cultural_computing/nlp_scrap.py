
# -------------------- FOOD SCRAPING

import pandas as pd
import numpy as np
import re
import requests
from bs4 import BeautifulSoup
import json
import matplotlib.pyplot as plt
import seaborn as sns
import nltk
import string
from gensim import corpora, models
from sklearn.manifold import TSNE
import folium
from folium.plugins import HeatMap

def get_menu_data(url, uid, cuisine):
  
    # Get HTML, parse it, and identify JSON containing Restaurant/Menu Data
    html = requests.get(url)
    soup = BeautifulSoup(html.text, "html.parser")
    restaurant_data = soup.find('script', type='application/ld+json')
    try:
        # set strict to False, so that control characters like \n are allowed within the text:
        restaurant_data_json = json.loads(restaurant_data.decode_contents(), strict = False)

        # Store each menu item (a dictionary) into nested_items variable for retrieving item names and descriptions below:
        nested_items = [section['hasMenuItem'] for section in restaurant_data_json['hasMenu'][0]['hasMenuSection']]

        # Store Data for individual menu/restaurant in a dictionary to enable easy aggregation into a Pandas DataFrame:
        # Item Descriptions are cleaned, so no $, stand-alone numbers, or measurements remain
        menu_data = {uid: {'Cuisine': cuisine,
                       'Restaurant': restaurant_data_json['name'],
                       'Coordinates': (restaurant_data_json['geo']['latitude'], restaurant_data_json['geo']['longitude']),
                       'Menu Items': [j['name'] for i in nested_items for j in i],
                       'Item Descriptions': [clean_description(j['description']) for i in nested_items for j in i],
                       'URL': url
                }}
    except:
        menu_data = {uid: {'Cuisine': cuisine,
                       'Restaurant': None,
                       'Coordinates': None,
                       'Menu Items': None,
                       'Item Descriptions': None,
                       'URL': url
                }}

    return menu_data

def clean_description(text):
    '''
    Removes prices, numbers, and measurements (in oz, liters) from input string, as well as other non-word characters
    '''
    clean = re.sub(r'(\$*\d+\.*\d*)|(\d*oz)|(liters*)|(&*amp;)|(comma;)', ' ', text)
    clean = re.sub(r'&#39;', "'", clean)
    return clean

def scrape_top_menus(cuisine_list, n=10):
    '''
    Scrapes the top `n` restaurant menus on `allmenus.com` from each cuisine in `cuisine_list`.
    
    Drops menus that are missing data.
    '''
    # Create a Dictionary for holding every cuisine's restaurant/menu data:
    cuisine_dict = {}
    # Incremental count for assigning unique identification numbers to each restaurant
    count = 0

    # Scrape top n (parameter n) restaurant links from allmenus.com for each cuisine:
    for cuisine in cuisine_list:
        # Get cuisine's popularity-sorted list of restaurants
        html = requests.get('https://www.allmenus.com/il/chicago/-/' + cuisine + '/')
        soup = BeautifulSoup(html.text, "html.parser")

        # Find links where class != grubhub (these take you to Grubhub's website, rather than the allmenus page we want)
        restaurant_anchors = soup.find('ul', class_="restaurant-list").findAll("a", class_=None)
        restaurant_links = ['https://www.allmenus.com' + i.get('href') for i in restaurant_anchors[:n]]

        # Loop through each Restaurant's allmenus.com webpage and get its menu data:
        menu_dict = {}
        for restaurant in restaurant_links:
            dict_temp = {}
            dict_temp = get_menu_data(url = restaurant,
                                      uid = count,
                                      cuisine = cuisine)
            menu_dict.update(dict_temp)
            count += 1
        cuisine_dict.update(menu_dict)

    # bring data into DataFrame and drop rows with missing data
    df = pd.DataFrame(cuisine_dict).T
    df.dropna(inplace = True)
    return df

def get_wordnet_pos(word):
    '''
    Tags each word with its Part-of-speech indicator -- specifically used for lemmatization in the get_lemmas function
    '''
    tag = nltk.pos_tag([word])[0][1][0].upper()
    tag_dict = {'J': nltk.corpus.wordnet.ADJ,
                'N': nltk.corpus.wordnet.NOUN,
                'V': nltk.corpus.wordnet.VERB,
                'R': nltk.corpus.wordnet.ADV}

    return tag_dict.get(tag, nltk.corpus.wordnet.NOUN)

def get_lemmas(text):
    '''
    Gets lemmas for a string input, excluding stop words, punctuation, as well as a set of study-specific stop-words
    '''
    # Define stop words
    stop = nltk.corpus.stopwords.words('english') + list(string.punctuation) + ['comma', 'amp', 'w/', 'w.', 'z']
    
    # Combine list elements together into a single string to use NLTK's tokenizer
    text = ' '.join(text)
    
    # tokenize + lemmatize words in text
    tokens = [i for i in nltk.word_tokenize(text.lower()) if i not in stop]
    lemmas = [nltk.stem.WordNetLemmatizer().lemmatize(t, get_wordnet_pos(t)) for t in tokens]
    return lemmas

def plot_top_tfidf(series, data_description, n = 10):
    '''
    Plots the top `n` TF-IDF words in a Pandas series of strings.
    '''
    # Get lemmas for each row in the input Series
    lemmas = series.apply(get_lemmas)

    # Initialize Series of lemmas as Gensim Dictionary for further processing
    dictionary = corpora.Dictionary([i for i in lemmas])

    # Convert dictionary into bag of words format: list of (token_id, token_count) tuples
    bow_corpus = [dictionary.doc2bow(text) for text in lemmas]

    # Calculate TFIDF based on bag of words counts for each token and return weights:
    tfidf = models.TfidfModel(bow_corpus)
    tfidf_weights = {}
    for doc in tfidf[bow_corpus]:
        for ID, freq in doc:
            tfidf_weights[dictionary[ID]] = np.around(freq, decimals = 2)

    # highest TF-IDF values:
    top_n = pd.Series(tfidf_weights).nlargest(n)

    # Plot the top n weighted words:
    plt.plot(top_n.index, top_n.values, label=data_description)
    plt.xticks(rotation='vertical')
    plt.title('Top {} Lemmas (TFIDF) for '.format(n) + data_description);

    return

def make_folium_pt_map(menu_df):
    
    m = folium.Map(zoom_start=12, location=[41.8507, -87.6340], tiles='CartoDB positron')

    for point in menu_df.index:
        popup_content = '<b>Restaurant: </b>' + menu_df['Restaurant'][point] + '\n' + '<b>Cuisine: </b>' + menu_df['Cuisine'][point].capitalize()
        
        # Colors are baked into the function, so we won't be able to reassign colors/cuisines without reorganizing:
        color_cuisine = {'italian': 'red','soul-food': 'blue', 'latin-american': 'green', 'gastropub': 'purple',
                         'polish': 'orange', 'late-night': 'darkred', 'thai': 'black', 'chinese': 'pink',
                         'korean': 'darkblue', 'vegan_vegetarian':'lightgreen'
                        }
        color_icon_body = color_cuisine[menu_df['Cuisine'][point]]

        folium.Marker(menu_df['Coordinates'][point],
                      popup=popup_content,
                      icon=folium.Icon(color=color_icon_body,icon_color='white', icon='cutlery')
                     ).add_to(m)

    legend_html = '''
             <div style="position: fixed;
             top: 50px; right: 50px; width: 150px; height: 325px;
             border:2px solid grey; z-index:9999; font-size:14px;
             ">&nbsp; Cuisine Color Code<br>
             &nbsp;<i class="fa fa-map-marker fa-2x" style="color:red"></i>&nbsp; Italian<br>
             &nbsp;<i class="fa fa-map-marker fa-2x" style="color:blue"></i>&nbsp; Soul Food<br>
             &nbsp;<i class="fa fa-map-marker fa-2x" style="color:green"></i>&nbsp; Latin American<br>
             &nbsp;<i class="fa fa-map-marker fa-2x" style="color:purple"></i>&nbsp; Gastropub<br>
             &nbsp;<i class="fa fa-map-marker fa-2x" style="color:orange"></i>&nbsp; Polish <br>
             &nbsp;<i class="fa fa-map-marker fa-2x" style="color:darkred"></i>&nbsp; Late Night<br>
             &nbsp;<i class="fa fa-map-marker fa-2x" style="color:black"></i>&nbsp; Thai<br>
             &nbsp;<i class="fa fa-map-marker fa-2x" style="color:pink"></i>&nbsp; Chinese<br>
             &nbsp;<i class="fa fa-map-marker fa-2x" style="color:darkblue"></i>&nbsp; Korean<br>
             &nbsp;<i class="fa fa-map-marker fa-2x" style="color:lightgreen"></i>&nbsp; Vegan/Vegetarian
             </div>
            '''
    m.get_root().html.add_child(folium.Element(legend_html))
    return m

def word2vec_tsne_plot(model, perplexity=40, n_iter=2500):
    '''
    Creates and TSNE model based on a Gensim word2vec model and plots it, 
    given parameter inputs of perplexity and number of iterations.
    '''
    labels = []
    tokens = []

    for word in model.wv.vocab:
        tokens.append(model.wv[word])
        labels.append(word)

    # Reduce 100 dimensional vectors down into 2-dimensional space so that we can see them
    tsne_model = TSNE(perplexity=perplexity, n_components=2, init='pca', n_iter=n_iter, random_state=23)
    new_values = tsne_model.fit_transform(tokens)

    x = []
    y = []
    for value in new_values:
        x.append(value[0])
        y.append(value[1])
    
    for i in range(len(x)):
        plt.scatter(x[i],y[i])
        plt.annotate(labels[i],
                     xy=(x[i], y[i]),
                     xytext=(5, 2),
                     textcoords='offset points',
                     ha='right',
                     va='bottom')
    
    plt.show()
    return

def doc2vec_tsne_plot(doc_model, labels, perplexity=40, n_iter=2500):
    '''
    Creates and TSNE model based on a Gensim doc2vec model and plots it, 
    given parameter inputs of perplexity and number of iterations.
    '''
    tokens = []
    for i in range(len(doc_model.docvecs.vectors_docs)):
        tokens.append(doc_model.docvecs.vectors_docs[i])

    # Reduce 100 dimensional vectors down into 2-dimensional space so that we can see them
    tsne_model = TSNE(perplexity=perplexity, n_components=2, init='pca', n_iter=n_iter, random_state=23)
    new_values = tsne_model.fit_transform(tokens)

    X = [doc[0] for doc in new_values]
    y = [doc[1] for doc in new_values]

    # Combine data into DataFrame, so that we plot it easily using Seaborn
    df = pd.DataFrame({'X':X, 'y':y, 'Cuisine':labels})
    plt.figure(figsize=(10, 10))
    sns.scatterplot(x="X", y="y", hue="Cuisine", data=df)
    return
  
  
# -------------------- NLP
  
  
import pandas as pd
import numpy as np
import nltk
import matplotlib.pyplot as plt
import string
from gensim import corpora, models

# Define stop words + punctuation + study-specific stop-words
STOP = nltk.corpus.stopwords.words('english') + list(string.punctuation) + ["amp", "39", "subscribe", "follow",
                                                                            "link", "ermenegildo", "zegna", "uomo",
                                                                            "music", "applause", "um", "facebook"
                                                                           ]

def pos_tag(text):
    '''
    Tags each word in a string with its part-of-speech indicator, excluding stop-words
    '''
    # Tokenize words using nltk.word_tokenize, keeping only those tokens that do not appear in the stop words we defined
    tokens = [i for i in nltk.word_tokenize(text.lower()) if i not in STOP]

    # Label parts of speech automatically using NLTK
    pos_tagged = nltk.pos_tag(tokens)
    return pos_tagged

def plot_top_adj(series, data_description, n = 15):
    '''
    Plots the top `n` adjectives in a Pandas series of strings.
    '''
    # Apply part of Speech tagger that we wrote above to any Pandas series that pass into the function
    pos_tagged = series.apply(pos_tag)

    # Extend list so that it contains all words/parts of speech for all the captions
    pos_tagged_full = []
    for i in pos_tagged:
        pos_tagged_full.extend(i)

    # Create Frequency Distribution of different adjectives and plot the distribution
    fd = nltk.FreqDist(word + "/" + tag for (word, tag) in pos_tagged_full if tag[:2] == 'JJ')
    fd.plot(n, title='Top {} Adjectives for '.format(n) + data_description);
    return

def get_wordnet_pos(word):
    '''
    Tags each word with its Part-of-speech indicator -- specifically used for lemmatization in the get_lemmas function
    '''
    tag = nltk.pos_tag([word])[0][1][0].upper()
    tag_dict = {"J": nltk.corpus.wordnet.ADJ,
                "N": nltk.corpus.wordnet.NOUN,
                "V": nltk.corpus.wordnet.VERB,
                "R": nltk.corpus.wordnet.ADV}

    return tag_dict.get(tag, nltk.corpus.wordnet.NOUN)

def get_lemmas(text):
    '''
    Gets lemmas for a string input, excluding stop words, punctuation, as well as a set of study-specific stop-words
    '''
    tokens = [i for i in nltk.word_tokenize(text.lower()) if i not in STOP]
    lemmas = [nltk.stem.WordNetLemmatizer().lemmatize(t, get_wordnet_pos(t)) for t in tokens]
    return lemmas

def plot_top_lemmas(series, data_description, n = 20):
    '''
    Plots the top `n` lemmas in a Pandas series of strings.
    '''
    lemmas = series.apply(get_lemmas)

    # Extend list so that it contains all words/parts of speech for all the captions
    lemmas_full = []
    for i in lemmas:
        lemmas_full.extend(i)

    nltk.FreqDist(lemmas_full).plot(n, title='Top {} Lemmas Overall for '.format(n) + data_description);
    return

def plot_top_tfidf(series, data_description, n = 15):
    '''
    Plots the top `n` TF-IDF words in a Pandas series of strings.
    '''
    # Get lemmas for each row in the input Series
    lemmas = series.apply(get_lemmas)

    # Initialize Series of lemmas as Gensim Dictionary for further processing
    dictionary = corpora.Dictionary([i for i in lemmas])

    # Convert dictionary into bag of words format: list of (token_id, token_count) tuples
    bow_corpus = [dictionary.doc2bow(text) for text in lemmas]

    # Calculate TFIDF based on bag of words counts for each token and return weights:
    tfidf = models.TfidfModel(bow_corpus)
    tfidf_weights = {}
    for doc in tfidf[bow_corpus]:
        for ID, freq in doc:
            tfidf_weights[dictionary[ID]] = np.around(freq, decimals = 2)

    # highest TF-IDF values:
    top_n = pd.Series(tfidf_weights).nlargest(n)

    # Plot the top n weighted words:
    plt.plot(top_n.index, top_n.values)
    plt.xticks(rotation='vertical')
    plt.title('Top {} Lemmas (TFIDF) for '.format(n) + data_description);

    return
  
 # -------------------- MUSIC 
  
import pandas as pd
import nltk
import string
from gensim import corpora, models
from gensim.utils import effective_n_jobs
import collections
import re

# Some Functions from Last Time to get us started:
def get_wordnet_pos(word):
    '''
    Tags each word with its Part-of-speech indicator -- specifically used for lemmatization in the get_lemmas function
    '''
    tag = nltk.pos_tag([word])[0][1][0].upper()
    tag_dict = {'J': nltk.corpus.wordnet.ADJ,
                'N': nltk.corpus.wordnet.NOUN,
                'V': nltk.corpus.wordnet.VERB,
                'R': nltk.corpus.wordnet.ADV}

    return tag_dict.get(tag, nltk.corpus.wordnet.NOUN)

def get_lemmas(text):
    '''
    Gets lemmas for a string input, excluding stop words, punctuation
    '''
    # Define stop words
    stop = nltk.corpus.stopwords.words('english') + list(string.punctuation)
    
    # Combine list elements together into a single string to use NLTK's tokenizer
    text = ' '.join(text)
    
    # tokenize + lemmatize words in text
    tokens = [i for i in nltk.word_tokenize(text.lower()) if i not in stop]
    lemmas = [nltk.stem.WordNetLemmatizer().lemmatize(t, get_wordnet_pos(t)) for t in tokens]
    return lemmas

def get_tokens(text):
    '''
    Gets all tokens (including stop words), excluding punctuation
    '''
    # drop punctuation, but keep stopwords for initial word counting
    text = text.translate(str.maketrans('', '', string.punctuation))

    # tokenize remaining words and make a list of them for input `text`
    tokens = [i for i in nltk.word_tokenize(text.lower())]
    return tokens

def social_connection(text, liwc_dict):
    '''
    Compute rel. percentage of LIWC 2007 'social' category:
    words like "mate," "talk," "child"
    '''
    
    liwc_counts = wordCount(text, liwc_dict)
    
    return liwc_counts[0]['social'] / liwc_counts[2]

def antisocial_perc(text, liwc_dict):
    '''
    Compute rel. percentage of LIWC 2007 'anger' and 'swear' categories:
    words like "kill," "hate," "annoyed," "damn," "fuck"
    '''
    liwc_counts = wordCount(text, liwc_dict)
    
    return (liwc_counts[0]['anger'] + liwc_counts[0]['swear']) / liwc_counts[2]

def positive_perc(text, liwc_dict):
    '''
    Compute rel. percentage of LIWC 2007 'posemo' categories:
    words like "love," "nice," "sweet"
    '''
    liwc_counts = wordCount(text, liwc_dict)
    
    return liwc_counts[0]['posemo'] / liwc_counts[2]

def compute_coherence_values(dictionary, corpus, texts, limit, start=2, step=2):
    '''
    Computes Coherence values for LDA models with differing numbers of topics.
    
    Returns list of models along with their respective coherence values (pick
    models with the highest coherence)
    '''
    coherence_values = []
    model_list = []
    for num_topics in range(start, limit, step):
        model = models.ldamulticore.LdaMulticore(corpus=corpus,
                                                 id2word=dictionary,
                                                 num_topics=num_topics,
                                                 workers=effective_n_jobs(-1))
        model_list.append(model)
        coherence_model = models.coherencemodel.CoherenceModel(model=model, 
                                                               corpus=corpus,
                                                               dictionary=dictionary,
                                                               coherence='u_mass')
        coherence_values.append(coherence_model.get_coherence())

    return model_list, coherence_values

def fill_topic_weights(df_row, bow_corpus, ldamodel):
    '''
    Fill DataFrame rows with topic weights for topics in songs.
    
    Modifies DataFrame rows *in place*.
    '''
    try:
        for i in ldamodel[bow_corpus[df_row.name]]:
            df_row[str(i[0])] = i[1]
    except:
        return df_row
    return df_row

def top_songs_by_topic(music_df, ldamodel, corpus, ntop=1):
    '''
    Finds the top "n" songs by topic, which we can use for
    understanding the types of songs included in a topic.
    '''
    topn_songs_by_topic = {}
    for i in range(len(ldamodel.print_topics())):
        # For each topic, collect the most representative song(s)
        # (i.e. highest probability containing words belonging to topic):
        top = sorted(zip(range(len(corpus)), ldamodel[corpus]), 
                     reverse=True, 
                     key=lambda x: abs(dict(x[1]).get(i, 0.0)))
        topn_songs_by_topic[i] = [j[0] for j in top[:ntop]]
        
        # Print out the topn songs for each topic and return their indices as a dictionary for further analysis:
        print("Topic " + str(i))
        print(music_df[['title','year','artist']].loc[topn_songs_by_topic[i]])
        print("*******************************")
    
    return topn_songs_by_topic
  
  
  # ---------- TBC
  
  
  
