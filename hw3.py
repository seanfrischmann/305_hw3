# ===========================================================================
# +++hw2.py+++|
# ____________|
#
# Author: Sean Frischmann
# Class: Cse 305
# ===========================================================================

import sys

def print_list(stack):
	for i, v in enumerate(stack):
		print v

def load(inputFile):
	buf = ''
	with open(inputFile) as fileIn:
		while True:
			contents = fileIn.read(1)
			if not contents:
				fileIn.close()
				break;
			if contents != '\n':
				buf = buf + contents
	return buf

def parse(user_input, stack, bindings):
	position = 0
	while position < len(user_input):
		if user_input[position] == '\n':
			user_input[position].replace('\n','')
		position = position+1
	isError = False
	isPrimitive = False
	isNegative = False
	isInsert = False
	if user_input == 'quit':
		sys.exit(0)
	try:
		temp = None
		if user_input[0] == '-':
			t = 1
			temp = ''
			while t < len(user_input):
				temp = temp + user_input[t]
				t = t+1
			if temp.isdigit():
				isNegative = True
		if user_input.isdigit() or isNegative:
			isInsert = True
			isNegative = False
		elif user_input[0] == '"':
			isInsert = True
		elif user_input == 'if':
			if len(stack) >= 3 and (stack[0] == ':true:' or stack[0] == ':false:'):
				if stack[0] == ':true:':
					stack.pop(1)
				else:
					stack.pop(2)
				stack.pop(0)
			else:
				isError = True
		elif user_input == 'bind':
			if (stack[1])[0].isalpha(): 
				if not bindings.has_key(stack[1]):
					bindings[stack[1]] = stack[0]
					stack.pop(1)
				else:
					isError = True
			else:
				isError = True
		elif user_input == ':true:' or user_input == ':false:':
			isInsert = True
		elif user_input == 'add':
			temp = int(stack[1]) + int(stack[0])
			isPrimitive = True
		elif user_input == 'sub':
			temp = int(stack[1]) - int(stack[0])
			isPrimitive = True
		elif user_input == 'div':
			if int(stack[1]) < 0 and int(stack[0]) < 0:
				temp = int(stack[1]) / int(stack[0])
				temp = temp + 1
			else:
				temp = int(stack[1]) / int(stack[0])
			isPrimitive = True
		elif user_input == 'rem':
			if int(stack[1]) < 0 and int(stack[0]) < 0:
				temp2 = int(stack[1]) / int(stack[0])
				temp2 = temp2 + 1
				temp = int(stack[1]) - (temp2*int(stack[0]))
			else:
				temp = int(stack[1]) % int(stack[0])
			isPrimitive = True
		elif user_input == 'mul':
			temp = int(stack[1]) * int(stack[0])
			isPrimitive = True
		elif user_input == 'neg':
			temp = int(stack[0]) * int(-1)
			stack.pop(0)
			temp = str(temp)
			stack.insert(0, temp)
			user_input = ''
		elif user_input == 'pop':
			stack.pop(0)
			user_input = ''
		elif user_input == 'exc':
			temp = stack[1]
			stack.pop(1)
			stack.insert(0 , temp)
			user_input = ''
		elif user_input[0].isalpha():
			while bindings.has_key(user_input):
				temp = bindings[user_input]
				user_input = temp
			isInsert = True
		else:
			isError = True
	except:
		isError = True
	if isPrimitive:
		stack.pop(0)
		stack.pop(0)
		temp = str(temp)
		stack.insert(0, temp)
		user_input = ''
	if isInsert:
		stack.insert(0, user_input)
		user_input = ''
	if isError:
		stack.insert(0, ':error:')

def repl():
	print('\nInterpreter\n'
			'Author: Sean Frischmann\n'
			'Class: Cse 305\n'
			'Incomplete Features:\n'
			'    functions and apply\n')
	stack = list()
	closures = list()
	bindings = dict()
	isPrompt = True
	isSpace = False
	isString = False
	isFile = False
	user_input = ''
	buf = ''
	while True:
		if isPrompt:
			buf = raw_input('repl> ')
		elif not isFile:
			buf = raw_input(' ')
		elif isFile:
			isFile = False
			isPrompt = True
		position=0
		while position < len(buf):
			if buf[position] == '"':
				if isString == True:
					isPrompt = True
					isString = False
				else:
					isString = True
					isPrompt = False
			if (buf[position] == ' ') and (not isString):
				isSpace = True
			else:
				user_input = user_input + buf[position]
			if isSpace or position == len(buf)-1:
				if user_input == 'load':
					if len(stack) > 0 and (stack[0])[0] == '"':
						try:
							inputFile = stack[0]
							inputFile = inputFile.replace('"','')
							buf = buf[position:len(buf)]
							buf = load(inputFile)+' '+':true:'+buf
							stack.pop(0)
							isFile = True
							isPrompt = False
							user_input = ''
							break;
						except:
							user_input = ':error:'
							continue
				elif not isString:
					parse(user_input,stack,bindings)
					user_input = ''
					isSpace = False
			position = position+1
		if isPrompt:
			print_list(stack)


repl()
