import epitran, csv, os.path

with open('/Users/soskuthy/Documents/Research/current/2017/u-fronting/u-fronting/data/raw_data/language_epitran.csv', 'rU') as csvfile:
	rdr = csv.reader(csvfile, delimiter=',', quotechar='"')
	c = 0
	for row in rdr:
		if c > 1:
			break
		if c != 0:
			path = os.path.join('/Users/soskuthy/Documents/Research/current/2017/u-fronting/u-fronting/data/raw_data/corpus_data', row[2])
			print(path)
		c += 1
		#with open()