import Grammar
from Grammar import lexer
from Grammar import parser

with open('test') as fileProg:
	code = fileProg.read()
lexer.input(code)
with open('Lexer_output.txt', 'w') as LexOutput:
    LexOutput.write("Now you can see matched tokens:\n")
    while True: 
        tok = lexer.token() 
        if not tok: 
            break
        LexOutput.write("LexToken(" + tok.type + ",'" + str(tok.value) + "'," + str(tok.lineno) + "," + str(tok.lexpos) + ")\n")
root = parser.parse(code, debug = False)
with open('Parser_errors.txt', 'w') as file_parse_error:
    file_parse_error.write('\n'.join(Grammar.parse_list_error))
if Grammar.parse_errors > 0:
    print("Errors during parsing! Check \"Parser_errors.txt\"")
else:
    print("Now you can see parsed tree:")
    print(root)

if Grammar.ml_error_list:
    print(Grammar.ml_error_list)
if not root.get('start'):
	Grammar.ml_errors += 1
	Grammar.ml_error_list.append('Ne naidena funciya s imenem "start"')
else:
	key = 'start'
	Grammar.ml_memory[key] = {}
	if isinstance(root[key][2], list):
		for i in root[key][2]:
			i.visit(key)
			if Grammar.ml_errors:
				print(Grammar.ml_error_list)
	else:
		root[key][2].visit(key)
	print(Grammar.ml_memory)
	print(len(Grammar.ml_memory))
