from textblob import TextBlob
from textblob.taggers import NLTKTagger
from textblob import Word
import csv
import sys
reload(sys)
sys.setdefaultencoding("latin1")

INFILE = '/Documents/GRADUATE_SCHOOL/Projects/SEMSOC/data/all_words.csv'
OUTFILE =  "all_words.csv"

nltk_tagger = NLTKTagger()
words = []
with open(INFILE, 'rb') as csvfile:
	all_words = csv.reader(csvfile)
	for row in all_words:
  		words.append(TextBlob(row[0], pos_tagger=nltk_tagger))

# POS
words_pos = []
for x in words:
 	#words_pos.append(x.tags)
 	words_pos.append(x.pos_tags)
 
# LEMMATIZE
words_lemma = []
for x in words:
	w = Word(x)
	words_lemma.append([x, w.lemmatize()])

# SENTIMENT
words_sent = []
for x in words:
	try:
		words_sent.append([x, x.sentiment])
	except:
		pass

#### WRITE TO CSV ###
myfile = open('../data/pos_' + OUTFILE, 'wb')
wr = csv.writer(myfile)
for x in words_pos:
	try:
		wr.writerow([x[0][0], x[0][1]])
	except:
		pass

myfile = open('../data/lemma_' + OUTFILE , 'wb')
wr = csv.writer(myfile)
for x in words_lemma:
	try:
		wr.writerow([x[0].words[0], x[1].words[0]])  
	except:
		pass

myfile = open('../data/sent_' + OUTFILE , 'wb')
wr = csv.writer(myfile)
for x in words_sent:
	try:
		wr.writerow([x[0].words[0], x[1].polarity, x[1].subjectivity])
	except:
		pass