import re
from board import *
from register import *
def parseConfigurationFile(conf_path):
	conf_file = open(conf_path, 'r')
	b = board()
	regcount = 0
	for l in conf_file:
		m = re.match("[^;]*", l) #ignore everything after comments
		line = m.group(0)
		if re.match("\s*@(.*)",line): #if it is the device information line
			tokens = re.split(",",re.match(".*@(.*)",line).group(1)) #only look at stuff after the @ sign
			tokens = [x.strip("\t \r\n") for x in tokens]
			b._name = tokens[0]
			b._id = int(tokens[1])
			b._path = tokens[2]
			b._baud_rate = tokens[3]
		elif re.match("\s*\d(.*)",line):
			tokens = re.split(",",line) #split on commas
			tokens = [x.strip("\t \r\n") for x in tokens]
			tf = lambda x: x == 'Y' or x == 'y' 
			name = tokens[2] if tokens[2].rfind("/") == -1 else tokens[2][tokens[2].rindex("/")+1::]  #Match everything after last /
			b._registers[regcount] = register(regcount, int(tokens[0]),tokens[1],name,tf(tokens[3]),tf(tokens[4]),tokens[5])

			regcount += int(tokens[0])
		elif re.match("\s*\#(.*)",line): #if it is the device information line
			tokens = re.split(",",re.match(".*\#(.*)",line).group(1)) #only look at stuff after the @ sign
			regcount = int(tokens[0])
		elif re.match("\s*\$(.*)",line): #dont do anything for $64 line
			pass
		elif re.match("^\s*$", line): #Don't do anything for blank lines
			pass 
		else:
			if __debug__: print "eek I don't understand this line :" + line
	return b

