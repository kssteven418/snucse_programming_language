import sys
sys.setrecursionlimit(150000)

def nmuch1(n, d):
	d[(n, 1)] = 1
	return 1

def nmuch10(n, d):
	if (n, 1) in d:
		x = d[(n, 1)]
	else:
		x = nmuch1(n, d)
	
	y = 0
	if n >= 10:
		if (n-10, 10) in d:
			y = d[(n-10, 10)]
		else:
			y = nmuch10(n-10, d)
	d[(n, 10)] = x+y
	return x+y

def nmuch100(n, d):
	if (n, 10) in d:
		x = d[(n, 10)]
	else:
		x = nmuch10(n, d)
	
	y = 0
	if n >= 100:
		if (n-100, 100) in d:
			y = d[(n-100, 100)]
		else:
			y = nmuch100(n-100, d)
	d[(n, 100)] = x+y
	return x+y

def nmuch500(n, d):
	if (n, 100) in d:
		x = d[(n, 100)]
	else:
		x = nmuch100(n, d)
	
	y = 0
	if n >= 500:
		if (n-500, 500) in d:
			y = d[(n-500, 500)]
		else:
			y = nmuch500(n-500, d)
	d[(n, 500)] = x+y
	return x+y

def nmuch1000(n, d):
	if (n, 500) in d:
		x = d[(n, 500)]
	else:
		x = nmuch500(n, d)
	y = 0
	if n >= 1000:
		if (n-1000, 1000) in d:
			y = d[(n-1000, 1000)]
		else:
			y = nmuch1000(n-1000, d)
	d[(n, 1000)] = x+y
	return x+y

def nmuch5000(n, d):
	if (n, 1000) in d:
		x = d[(n, 1000)]
	else:
		x = nmuch1000(n, d)
	
	y = 0
	if n >= 5000:
		if (n-5000, 5000) in d:
			y = d[(n-5000, 5000)]
		else:
			y = nmuch5000(n-5000, d)
	d[(n, 5000)] = x+y
	return x+y

def nmuch10000(n, d):
	if (n, 5000) in d:
		x = d[(n, 5000)]
	else:
		x = nmuch5000(n, d)
	
	y = 0
	if n >= 10000:
		if (n-10000, 10000) in d:
			y = d[(n-10000, 10000)]
		else:
			y = nmuch10000(n-10000, d)
	d[(n, 10000)] = x+y
	return x+y

def nmuch50000(n, d):
	if (n, 10000) in d:
		x = d[(n, 10000)]
	else:
		x = nmuch10000(n, d)
	
	y = 0
	if n >= 50000:
		if (n-50000, 50000) in d:
			y = d[(n-50000, 50000)]
		else:
			y = nmuch50000(n-50000, d)
	d[(n, 50000)] = x+y
	return x+y

while(True):
	name = raw_input("number : ")
	num = int(name)
	d = {}
	print( nmuch50000(num, d))

