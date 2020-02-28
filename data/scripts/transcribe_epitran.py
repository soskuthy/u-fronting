import epitran, csv, os.path, codecs

with open('/Users/soskuthy/Documents/Research/current/2017/u-fronting/u-fronting/data/raw_data/language_epitran_2.csv', 'rU') as csvfile:
	rdr = csv.reader(csvfile, delimiter=',', quotechar='"')
	c = 0
	for row in rdr:
		if c >  0:
			epi = epitran.Epitran(row[3])
			path = os.path.join('/Users/soskuthy/Documents/Research/current/2017/u-fronting/u-fronting/data/raw_data/corpus_data', row[2])
			print(path)
			with codecs.open(path, 'r', encoding='utf8') as text_file:
				txt = [x.strip().split(" ") for x in text_file.readlines()]
				for l in txt:
					l[0] = epi.transliterate(l[0])
				new_path_l = os.path.splitext(path)
				new_path = new_path_l[0] + "-ipa" + new_path_l[1]
				with codecs.open(new_path, 'w', encoding='utf8') as out:
					out.writelines([' '.join(x) + "\n" for x in txt])
		c += 1
		#with open()