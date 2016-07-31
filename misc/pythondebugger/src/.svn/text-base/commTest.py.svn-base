import confParser, serialTalker, threading, time


b = confParser.parseConfigurationFile("conf/actuator.conf")
st = serialTalker.serialTalker(b, "/dev/ttyUSB0")

lock = threading.Lock()

print b._registers
count = 0
sleep_len = .1
while(1):
	while(count < 245):
		time.sleep(sleep_len)
		st.writeRegister(b._registers[17],count,lock)
		count += 10
	while(count > 1):
		time.sleep(sleep_len)
		st.writeRegister(b._registers[17],count,lock)
		count -= 10
st.writeRegister(b._registers[17],0,lock)
'''
print b._registers

print st.readRegister(b._registers[4], lock), st.readRegister(b._registers[5], lock)
st.writeRegister(b._registers[4], 2122, lock)
print st.readRegister(b._registers[4], lock), st.readRegister(b._registers[5], lock)
'''


'''
val = st.readRegister(b._registers[24],lock)
print val

print serialTalker.toHex(st.writeRegister(b._registers[24],21337,lock))

val = st.readRegister(b._registers[24],lock)
print val

val = st.readRegister(b._registers[1], lock)
print val
val2 = st.readRegister(b._registers[41], lock)
print val2
print serialTalker.toHex( st.writeRegister(b._registers[41],203.31,lock))
val2 = st.readRegister(b._registers[41], lock)
print val2
'''
