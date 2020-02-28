import phonemizer, csv, os.path, codecs, re
from phonemizer.phonemize import phonemize

with open('/Users/soskuthy/Documents/Research/current/2017/u-fronting/u-fronting/data/raw_data/language_phonemize_2.csv', newline="") as csvfile:
	rdr = csv.reader(csvfile, delimiter=',', quotechar='"')
	c = 0
	for row in rdr:
		print(row)
		if c > 0:
			language = row[3]
			backend = row[4]
			#phonemizer.phonemize.phonemize("hello", language="fr-fr", backend="espeak").strip()
			path = os.path.join('/Users/soskuthy/Documents/Research/current/2017/u-fronting/u-fronting/data/raw_data/corpus_data', row[2])
			print(path)
			with codecs.open(path, 'r', encoding='utf8') as text_file:
				txt = [x.strip().split(" ") for x in text_file.readlines()]
				cc = 0
				txt = [x for x in txt if not re.search('(.)\\1{2,}', x[0]) and not re.search('[!"#$%&()*+,./:;<=>?@\\[\\]\\^\\\\_{|}~]', x[0]) and not x[0]==""]
				words = [x[0] for x in txt]
				counts = [x[1] for x in txt]
				words_phon = [x.strip() for x in phonemizer.phonemize.phonemize(words, language=language, backend=backend)]
				txt_out = list(zip(words_phon, counts))
				new_path_l = os.path.splitext(path)
				new_path = new_path_l[0] + "-ipa" + new_path_l[1]
				with codecs.open(new_path, 'w', encoding='utf8') as out:
					out.writelines([' '.join(x) + "\n" for x in txt_out])
		c += 1
		#with open()