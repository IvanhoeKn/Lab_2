import ply.lex as lex
import ply.yacc as yacc
import re
import copy
import math

reserved = {
    'const' : 'CONST',
    'signed' : 'SIGNED',
    'unsigned' : 'UNSIGNED',
    'cell' : 'CELL',
    'top' : 'TOP',
    'ntop' : 'NTOP',
    'right' : 'RIGHT',
    'nright' : 'NRIGHT',
    'down' : 'DOWN',
    'ndown' : 'NDOWN',
    'left' : 'LEFT',
    'nleft' : 'NLEFT',
    'matrix' : 'MATRIX',
    'testrep' : 'TESTREP',
    'testonce' : 'TESTONCE',
    'xray' : 'XRAY',
    'func' : 'FUNC',
    'call' : 'CALL'
}

tokens = ['ASSIGN', 'PLUS', 'MINUS', 'MUL', 'DIV', 'REM', 'EQUAL', 'GREAT', 'LESS',
    'UNOP', 'OPENSBR', 'CLOSESBR', 'OPENBR', 'CLOSEBR', 'COMMA', 'SEMICOLON',
    'COV', 'OPENBRACE', 'CLOSEBRACE', 'INT', 'ID'] + list(reserved.values())

t_PLUS = r'\+'
t_MINUS = r'-'
t_MUL = r'\*'
t_DIV = r'/'
t_REM = r'\%'
t_EQUAL = r'='
t_GREAT = r'>'
t_LESS = r'<'
t_UNOP = r'\#'
t_OPENSBR = r'\['
t_CLOSESBR = r'\]'
t_OPENBR = r'\('
t_CLOSEBR = r'\)'
t_COMMA = r','
t_SEMICOLON = r';'
t_COV = r'\''
t_OPENBRACE = r'\{'
t_CLOSEBRACE = r'\}'

def t_ASSIGN(t):
	r'<-'
	t.value = str(t.value)
	return t

def t_INT(t):
	r'[+-]?\d+'
	t.value = int(t.value)
	return t

def t_ID(t):
	r'[A-Za-z][A-Za-z0-9_]*'
	t.type = reserved.get(t.value, 'ID')
	t.value = str(t.value)
	return t 

#A newline rule 
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# A string containing ignored characters (spaces and tabs)
t_ignore = ' \r\t\f'

#Error handling rule
def t_error(t):
    #print("Illegal character %s" % t.value[0])
    print("Illegal character %s" % repr(t.value[0]))
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex(debug = 0)

######################################################################################

parse_errors = 0
parse_list_error = []

precedence = (
	('left', 'PLUS', 'MINUS'),
	('left', 'GREAT', 'LESS', 'EQUAL'),
	('left', 'MUL', 'DIV','REM')
)

def p_func(p):
	'''func : 
			| FUNC ID OPENBR argumentList CLOSEBR OPENBR statementGroup CLOSEBR func'''
	if len(p) > 1:
		if not p[9].get(p[2]):
			tmp = [p[2], p[4], p[7]]
			p[9][p[2]] = tmp
			p[0] = p[9]
		else:
			global parse_errors
			global parse_list_error
			parse_errors += 1
			parse_list_error.append('Function with the same name is already defined: ' + p[2])
	else:
		p[0] = {}

def p_argumentList(p):
	'''argumentList : 
				    | notEmptyArgumentList'''
	if len(p) > 1:
		p[0] = p[1]
	else:
		p[0] = None

def p_notEmptyArgumentList(p):                                  
	'''notEmptyArgumentList : declaration
                            | matrix
				            | declaration COMMA notEmptyArgumentList
                            | matrix COMMA notEmptyArgumentList'''
	if len(p) == 2:
		tmp = p[1]
		p[0] = [tmp]
	elif len(p) > 2:
		tmp = p[1]
		p[0] = [tmp] + p[3]

def p_declaration(p):
	'''declaration : typeList ID
			       | constTypeList ID'''
	p[0] = declaration(p[1], p[2])

def p_typeList(p):
	'''typeList : SIGNED
				| UNSIGNED
				| CELL'''
	p[0] = typeList(p[1])

def p_constTypeList(p):
	'''constTypeList : CONST SIGNED
					 | CONST UNSIGNED
					 | CONST CELL'''
	p[0] = constTypeList(p[2])

def p_statementGroup(p):                  
	'''statementGroup :
			          | statement statementGroup'''
	if len(p) > 1:
		if p[2]:
			p[0] = [p[1]] + p[2]
		else:
			p[0] = [p[1]]
	else:
		p[0] = None

def p_statement_Top(p):
	'''statement : TOP'''
	p[0] = Top(p[1])

def p_statement_Right(p):
	'''statement : RIGHT'''
	p[0] = Right(p[1])

def p_statement_Left(p):
	'''statement : LEFT'''
	p[0] = Left(p[1])

def p_statement_Down(p):
	'''statement : DOWN'''
	p[0] = Down(p[1])

def p_statement(p):
	'''statement : sentence
			     | testOnce
			     | testRep'''
	p[0] = p[1]

def p_testonce(p):														
	'''testOnce : TESTONCE OPENBR binaryExpression CLOSEBR OPENBR statementGroup CLOSEBR'''
	p[0] = testonce(p[1], p[3], p[6])

def p_testrep(p):
	'''testRep : TESTREP OPENBR binaryExpression CLOSEBR OPENBR statementGroup CLOSEBR'''							
	p[0] = testrep(p[1], p[3], p[6])

def p_sentence(p):
	'''sentence : expression SEMICOLON
			    | declaration SEMICOLON
			    | matrix SEMICOLON'''
	p[0] = p[1]

def p_expression_Id(p):
	'''expression : ID'''
	p[0] = Id(p[1])

def p_expression_Signt(p):
	'''expression : INT 
			     | negative'''
	p[0] = Int(p[1])

def p_negativeative(p):
	'''negative : MINUS expression'''
	p[0] = Negative(p[2])

def p_expression_Cell(p):
	'''expression : cell'''
	p[0] = p[1]

def p_cell(p):
	'''cell : OPENBR robotexpression COMMA robotexpression COMMA robotexpression COMMA robotexpression  CLOSEBR'''  
	p[0] = cell(p[2], p[4], p[6], p[8])

def p_expression_matrix(p):
	'''expression : matrixOneMore'''
	p[0] = p[1]

def p_matrixOneMore(p):
	'''matrixOneMore : OPENSBR array CLOSESBR'''
	p[0] = matrixOneMore(p[2])

def p_expression(p):
	'''expression : binaryExpression
			     | call
			     | assignment
			     | access
			     | unop
			     | xray'''
	p[0] = expression(p[1])

def p_matr(p):
	'''matrix : MATRIX declaration OPENSBR expression COMMA expression CLOSESBR
			  | MATRIX declaration'''
	if len(p) > 7:
		p[0] = matrixFirst(p[2], p[4], p[6])

	else:
		p[0] = matrixSecond(p[2])

def p_xray(p):
	'''xray : XRAY'''
	p[0] = xray(p[1])

def p_call(p):
	'''call : CALL ID OPENBR namelist CLOSEBR'''    
	p[0] = Call(p[2], p[4])

def p_namelist(p):
	'''namelist : 
				| notEmptyNamelist'''
	if len(p) > 1:
		p[0] = p[1]
	else:
		p[0] = None

def p_nep_namelist(p):
	'''notEmptyNamelist : expression
					    | expression COMMA notEmptyNamelist'''
	if len(p) == 2:
		tmp = p[1]
		p[0] = [tmp]
	elif len(p) > 2:
		tmp = p[1]
		p[0] = [tmp] + p[3]

def p_assignment(p):
	'''assignment : expression ASSIGN expression
				  | declaration ASSIGN expression
				  | matrix ASSIGN expression'''		
	p[0] = assignment(p[3], p[1], p[2])

def p_unop(p):
	'''unop : UNOP ID'''
	p[0] = unop(p[2], p[1])

def p_access(p):
	'''access : expression OPENBR expression COMMA expression CLOSEBR
			  | expression OPENBR expression COMMA expression COMMA expression CLOSEBR'''
	if len(p) == 7:
		p[0] = access(p[1], p[3], p[5], three = None)
	else:
		p[0] = access(p[1], p[3], p[5], p[7])

def p_binaryExpression(p):
    '''binaryExpression : 
				| expression PLUS expression
				| expression MINUS expression
				| expression MUL expression
				| expression DIV expression
				| expression REM expression
				| expression EQUAL expression
				| expression GREAT expression
				| expression LESS expression
				| expression EQUAL forRobotExpression'''
    if len(p) > 1:
	    p[0] = binaryExpression(p[1], p[2], p[3])

def p_forRobotExpression(p):
	'''forRobotExpression : COV robotexpression COV'''
	p[0] = forRobotExpression(p[2])

def p_robotexpression(p):
	'''robotexpression : TOP
			          | NTOP
			          | RIGHT
			          | NRIGHT
			          | LEFT
			          | NLEFT
			          | DOWN
			          | NDOWN'''
	p[0] = p[1]

def p_array(p):
	'''array : OPENSBR namelist CLOSESBR
			 | OPENSBR namelist CLOSESBR COMMA array'''
	if len(p) == 4:
		p[0] = [p[2]]
	else:
		tmp = p[2]
		p[0] = [tmp] + p[5]

# Error rules for syntax errors
def p_error(p):
    global parse_errors
    global parse_list_error
    parse_errors += 1
    parse_list_error.append("Error: Unexpected token {" + p.type + "}({" + str(p.value) + "}) on line {" + str(p.lineno) + "}")

def p_error_sentence(p):
    '''sentence : expression
	            | declaration
	            | matrix'''
    global parse_errors
    global parse_list_error
    parse_errors += 1
    parse_list_error.append("Expected semicolon on line {" + str(p.lineno) + "}")
    p[0] = None

def p_error_func(p):
    '''func : FUNC OPENBR argumentList CLOSEBR OPENBR statementGroup CLOSEBR func'''
    global parse_errors
    global parse_list_error
    parse_errors += 1
    parse_list_error.append("Missing ID in function declaration on line {" + str(p.lineno) + "}")
    p[0] = []

def p_error_call(p):
    '''call : CALL ID namelist'''
    global parse_errors
    global parse_list_error
    parse_errors += 1
    parse_list_error.append("Missing brackets: \'(\' \')\' in call on line {" + str(p.lineno) + "}")
    p[0] = None

def p_error_cell(p):
    '''cell : OPENBR robotexpression COMMA robotexpression COMMA robotexpression CLOSEBR''' 
    global parse_errors
    global parse_list_error
    parse_errors += 1
    parse_list_error.append("Not enough arguments for cell on line {"  + str(p.lineno) + "}")
    p[0] = None

# Build the parser
parser = yacc.yacc()  

######################################################################################

last_ret = None
last_declaration_var = None
robot_vis = []
ml_errors = 0
ml_error_list = []
ml_memory = {}
ml_memory[None] = {}

class declaration:
    def __init__(self, varType, varID):
        self.varType = varType
        self.varID = varID
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        global last_declaration_var
        varType = self.varType.visit(key)
        if ml_errors:
            return
        if ml_memory[key].get(self.varID):
            ml_errors += 1
            ml_error_list.append("A variable with the same name already exists \"" + self.varID + "\"")
            return
        else:
            ml_memory[key][self.varID] = [varType]
            #to store the value of a variable in the future: 
            ml_memory[key][self.varID].append([])
            ml_memory[key][self.varID][1] = None
            last_declaration_var = copy.copy(self.varID)

class typeList:
    def __init__(self, varType):
        self.varType = varType
    def visit(self, key):
        global ml_errors
        global ml_error_list
        if self.varType == 'signed':
            return 'signed'
        elif self.varType == 'unsigned':
            return 'unsigned'
        elif self.varType == 'cell':
            return 'cell'
        elif self.varType == 'matrix':
            return 'matrix'
        else:
            ml_errors += 1
            ml_error_list.append("Invalid variable type selected \"" + self.varType + "\"")
            return

class constTypeList:
    def __init__(self, varType):
        self.varType = varType
    def visit(self, key):
        global ml_errors
        global ml_error_list
        if self.varType == 'signed':
            return 'const signed'
        elif self.varType == 'unsigned':
            return 'const unsigned'		
        elif self.varType == 'cell':
            return 'const cell'
        elif self.varType == 'matrix':
            ml_errors += 1
            ml_error_list.append("Invalid specifier for matrix")
            return
        else:
            ml_errors += 1
            ml_error_list.append("Invalid variable type selected \"" + self.varType + "\"")
            return

class Id:
    def __init__(self, varName):
        self.varName = varName
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        global last_declaration_var
        if ml_errors:
            return
        if not ml_memory[key].get(self.varName):
            ml_errors += 1
            ml_error_list.append("This variable was not declared earlier \"" + self.varName + "\"")
            return
        varType_Value = ml_memory[key][self.varName]
        last_declaration_var = copy.copy(self.varName)
        return varType_Value

class Int:
    def __init__(self, number):
        self.number = number
    def visit(self, key):
        global ml_errors
        if ml_errors:
            return
        return ['int', self.number]

class Negative:
    def __init__(self, expressionArgument):
        self.expressionArgument = expressionArgument
    def visit(self, key):
        global ml_errors
        global ml_error_list    
        if ml_errors:
            return
        varType_Value = self.expressionArgument.visit(key)
        if varType_Value[0] == 'signed':
            vr[1] = -vr[1]
            return vr
        else:
            ml_errors += 1
            ml_error_list.append('otritsatelnoe znachenie ne sushestvuet dlya dannih virazheniy')######
            return

class expression:
    def __init__(self, expressionArgument):
        self.expressionArgument = expressionArgument
    def visit(self, key):
        global ml_errors
        if ml_errors:
            return
        return self.expressionArgument.visit(key)

class assignment:
    def __init__(self, right, var, assignment):
        self.right = right		
        self.var = var
        self.assignment = assignment
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        global last_declaration_var
        right = self.right.visit(key)
        var = self.var.visit(key)
        tmp = copy.deepcopy(right)
        if ml_errors:
            return
        if not ml_memory[key].get(last_declaration_var):
            ml_errors += 1
            ml_error_list.append('peremennaya ne declarirovana')
            return
        if ml_memory[key][last_declaration_var][1] != None and (ml_memory[key][last_declaration_var][0] == 'const signed' or ml_memory[key][last_declaration_var][0] == 'const unsigned' or ml_memory[key][last_declaration_var][0] == 'const cell'):
            ml_errors += 1
            ml_error_list.append('Nelzya izmenit constanty')
            return
        else:
            if ml_memory[key][last_declaration_var][0] == 'signed' or (ml_memory[key][last_declaration_var][1] == None and ml_memory[key][last_declaration_var][0] == 'const signed'):
                if right[0] == 'cell' or right[0] == 'const cell' or right[0] == 'element':
                    tmp = cast_Cell_SgnUnSgn(tmp)
                elif right[0] == 'matrix_Sgn' or right[0] == 'matrix_UnSgn' or right[0] == 'matrix_Cell':
                    ml_errors += 1
                    ml_error_list.append('nesootvetstvie tipov')
                    return
            elif ml_memory[key][last_declaration_var][0] == 'unsigned' or (ml_memory[key][last_declaration_var][1] == None and ml_memory[key][last_declaration_var][0] == 'const unsigned'):
                if right[0] == 'signed' or right[0] == 'const signed' or right[0] == 'int':
                    tmp = cast_integerVar_UnSgn(tmp)                
                elif right[0] == 'cell' or right[0] == 'const cell' or right[0] == 'element':
                    tmp = cast_Cell_SgnUnSgn(tmp)
                elif right[0] == 'matrix_Sgn' or right[0] == 'matrix_UnSgn' or right[0] == 'matrix_Cell':
                    ml_errors += 1
                    ml_error_list.append('nesootvetstvie tipov')
                    return
            elif ml_memory[key][last_declaration_var][0] == 'cell' or (ml_memory[key][last_declaration_var][1] == None and ml_memory[key][last_declaration_var][0] == 'const cell'):
                if right[0] == 'signed' or right[0] == 'const signed' or right[0] == 'unsigned' or right[0] == 'const unsigned' or right[0] == 'int':
                    tmp = cast_integerVar_Cell(tmp)
                elif right[0] == 'matrix_Sgn' or right[0] == 'matrix_UnSgn' or right[0] == 'matrix_Cell':
                    ml_errors += 1
                    ml_error_list.append('nesootvetstvie tipov')
                    return
            elif ml_memory[key][last_declaration_var][0] == 'matrix_Sgn' or ml_memory[key][last_declaration_var][0] == 'matrix_int':
                if right[0] == 'matrix_Cell' or right[0] == 'matrix_element':
                    for i in range(len(tmp[1])):
                            for j in range(len(tmp[1][0])):
                                tmp[1][i][j] = cast_Cell_SgnUnSgn(tmp[1][i][j])
                elif right[0] == 'signed' or right[0] == 'const signed' or right[0] == 'unsigned' or right[0] == 'const unsigned' or right[0] == 'cell' or right[0] == 'const cell' or right[0] == 'int' or right[0] == 'element':
                    ml_errors += 1
                    ml_error_list.append('nesootvetstvie tipov')
                    return
            elif ml_memory[key][last_declaration_var][0] == 'matrix_UnSgn':
                if right[0] == 'matrix_Sgn' or right[0] == 'matrix_int':
                    for i in range(len(tmp[1])):
                            for j in range(len(tmp[1][0])):
                                tmp[1][i][j] = cast_integerVar_UnSgn(tmp[1][i][j])
                                if ml_errors:
                                    return
                elif right[0] == 'matrix_Cell' or right[0] == 'matrix_element':
                    for i in range(len(tmp[1])):
                            for j in range(len(tmp[1][0])):
                                tmp[1][i][j] = cast_Cell_UnSgn(tmp[1][i][j])
                elif right[0] == 'signed' or right[0] == 'const signed' or right[0] == 'unsigned' or right[0] == 'const unsigned' or right[0] == 'cell' or right[0] == 'const cell' or right[0] == 'int' or right[0] == 'element':
                    ml_errors += 1
                    ml_error_list.append('nesootvetstvie tipov')
                    return
            elif ml_memory[key][last_declaration_var][0] == 'matrix_Cell' or ml_memory[key][last_declaration_var][0] == 'matrix_element':
                if right[0] == 'matrix_Sgn' or right[0] == 'matrix_UnSgn' or right[0] == 'matrix_int':
                    for i in range(len(tmp[1])):
                            for j in range(len(tmp[1][0])):
                                tmp[1][i][j] = cast_integerVar_Cell(tmp[1][i][j])
                elif right[0] == 'signed' or right[0] == 'const signed' or right[0] == 'unsigned' or right[0] == 'const unsigned' or right[0] == 'cell' or right[0] == 'const cell' or right[0] == 'int' or right[0] == 'element':
                    ml_errors += 1
                    ml_error_list.append('nesootvetstvie tipov')
                    return
            ml_memory[key][last_declaration_var][1] = copy.deepcopy(tmp[1])
            return ml_memory[key][last_declaration_var]
            
class binaryExpression:
    def __init__(self, left, operator, right):
        self.left = left
        self.right = right
        self.operator = operator
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        global last_ret
        tmp = None        
        result = None
        matrix_result = []
        list_result = []
        tmp_matrix = []
        summ = 0
        operand_1 = self.left.visit(key)
        if ml_errors:
            return
        operand_2 = self.right.visit(key)
        if ml_errors:
            return
        ########Operator slojeniya############
        if self.operator == '+':
            if operand_1[0] == 'signed' or operand_1[0] == 'const signed' or operand_1[0] == 'int':
                if operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned' or operand_2[0] == 'int':
                    last_ret = [operand_1[0], operand_1[1] + operand_2[1]]
                    return [operand_1[0], operand_1[1] + operand_2[1]]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_2[0] == 'element':
                    tmp = copy.copy(operand_2)
                    tmp = cast_Cell_SgnUnSgn(tmp)
                    last_ret = [operand_1[0], operand_1[1] + tmp[1]]
                    return [operand_1[0], operand_1[1] + tmp[1]]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'unsigned' or operand_1[0] == 'const unsigned':
                if operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned':
                    last_ret = [operand_1[0], operand_1[1] + operand_2[1]]
                    return [operand_1[0], operand_1[1] + operand_2[1]]
                elif operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'int': 
                    tmp = copy.copy(operand_2)
                    tmp = cast_integerVar_UnSgn(tmp)
                    if ml_errors:
                        return
                    else:
                        last_ret = [operand_1[0], operand_1[1] + tmp[1]]
                        return [operand_1[0], operand_1[1] + tmp[1]]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_2[0] == 'element':
                    tmp = copy.copy(operand_2)
                    tmp = cast_Cell_SgnUnSgn(tmp)
                    last_ret = [operand_1[0], operand_1[1] + tmp[1]]
                    return [operand_1[0], operand_1[1] + tmp[1]]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'cell' or operand_1[0] == 'const cell' or operand_1[0] == 'element':
                if operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned' or operand_2[0] == 'int':
                    tmp = copy.copy(operand_2)
                    tmp = cast_integerVar_Cell(tmp)
                    list_result = cell_OR(operand_1[1], tmp[1])
                    last_ret = [operand_1[0], list_result]
                    return [operand_1[0], list_result]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_1[0] == 'element':
                    list_result = cell_OR(operand_1[1], operand_2[1])
                    last_ret = [operand_1[0], list_result]
                    return [operand_1[0], list_result]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'matrix_Sgn' or operand_1[0] == 'matrix_int':
                if operand_2[0] == 'matrix_Sgn' or operand_2[0] == 'matrix_UnSgn' or operand_2[0] == 'matrix_int':
                    if (len(operand_1[1]) != len(operand_2[1])) or (len(operand_1[1][0]) != len(operand_2[1][0])):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                matrix_result[1][i][j][1] += operand_2[1][i][j][1]
                        last_ret = matrix_result
                        return matrix_result
                elif operand_2[0] == 'matrix_Cell' or operand_2[0] == 'matrix_element':
                    if len(operand_1[1]) != len(operand_2[1]) or len(operand_1[1][0]) != len(operand_2[1][0]):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                tmp = operand_2[1][i][j]
                                tmp = cast_Cell_SgnUnSgn(tmp)
                                matrix_result[1][i][j][1] += tmp[1]
                        last_ret = matrix_result
                        return matrix_result
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'matrix_UnSgn':
                if operand_2[0] == 'matrix_Sgn' or operand_2[0] == 'matrix_int':
                    if len(operand_1[1]) != len(operand_2[1]) or len(operand_1[1][0]) != len(operand_2[1][0]):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                tmp = operand_2[1][i][j]
                                tmp = cast_integerVar_UnSgn(tmp)
                                if ml_errors:
                                    return
                                matrix_result[1][i][j][1] += tmp[1]
                        last_ret = matrix_result
                        return matrix_result
                elif operand_2[0] == 'matrix_UnSgn':
                    if len(operand_1[1]) != len(operand_2[1]) or len(operand_1[1][0]) != len(operand_2[1][0]):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                matrix_result[1][i][j][1] += operand_2[1][i][j][1]
                        last_ret = matrix_result
                        return matrix_result
                elif operand_2[0] == 'matrix_Cell' or operand_2[0] == 'matrix_element':
                    if len(operand_1[1]) != len(operand_2[1]) or len(operand_1[1][0]) != len(operand_2[1][0]):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                tmp = operand_2[1][i][j]
                                tmp = cast_Cell_SgnUnSgn(tmp)
                                matrix_result[1][i][j][1] += tmp[1]
                        last_ret = matrix_result
                        return matrix_result
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'matrix_Cell' or operand_1[0] == 'matrix_element':
                if operand_2[0] == 'matrix_Sgn' or operand_2[0] == 'matrix_UnSgn' or operand_2[0] == 'matrix_int':
                    if len(operand_1[1]) != len(operand_2[1]) or len(operand_1[1][0]) != len(operand_2[1][0]):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                tmp = operand_2[1][i][j]
                                tmp = cast_integerVar_Cell(tmp)
                                matrix_result[1][i][j][1] = cell_OR(matrix_result[1][i][j][1], tmp[1])
                        last_ret = matrix_result
                        return matrix_result
                elif operand_2[0] == 'matrix_Cell' or operand_2[0] == 'matrix_element':
                    if len(operand_1[1]) != len(operand_2[1]) or len(operand_1[1][0]) != len(operand_2[1][0]):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                matrix_result[1][i][j][1] = cell_OR(matrix_result[1][i][j][1], operand_2[1][i][j][1])
                        last_ret = matrix_result
                        return matrix_result
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
        ########Operator vichitaniya############
        if self.operator == '-':
            if operand_1[0] == 'signed' or operand_1[0] == 'const signed' or operand_1[0] == 'int':
                if operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned' or operand_2[0] == 'int':
                    last_ret = [operand_1[0], operand_1[1] - operand_2[1]]
                    return [operand_1[0], operand_1[1] - operand_2[1]]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_2[0] == 'element':
                    tmp = copy.copy(operand_2)
                    tmp = cast_Cell_SgnUnSgn(tmp)
                    last_ret = [operand_1[0], operand_1[1] - tmp[1]]
                    return [operand_1[0], operand_1[1] - tmp[1]]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'unsigned' or operand_1[0] == 'const unsigned':
                if operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned':
                    if operand_1[1] - operand_2[1] < 0:
                        ml_errors += 1 
                        ml_error_list.append('vihod za razryadnuyu setku')
                        return
                    last_ret = [operand_1[0], operand_1[1] - operand_2[1]]
                    return [operand_1[0], operand_1[1] - operand_2[1]]
                elif operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'int': 
                    tmp = copy.copy(operand_2)
                    tmp = cast_integerVar_UnSgn(tmp)
                    if ml_errors:
                        return
                    if operand_1[1] - tmp[1] < 0:
                        ml_errors += 1 
                        ml_error_list.append('vihod za razryadnuyu setku')
                        return
                    last_ret = [operand_1[0], operand_1[1] - tmp[1]]
                    return [operand_1[0], operand_1[1] - tmp[1]]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_2[0] == 'element':
                    tmp = copy.copy(operand_2)
                    tmp = cast_Cell_SgnUnSgn(tmp)
                    if operand_1[1] - tmp[1] < 0:
                        ml_errors += 1 
                        ml_error_list.append('vihod za razryadnuyu setku')
                        return
                    last_ret = [operand_1[0], operand_1[1] - tmp[1]]
                    return [operand_1[0], operand_1[1] - tmp[1]]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'cell' or operand_1[0] == 'const cell' or operand_1[0] == 'element':
                if operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned' or operand_2[0] == 'int':
                    tmp = copy.copy(operand_2)
                    tmp = cast_integerVar_Cell(tmp)
                    list_result = cell_XOR(operand_1[1], tmp[1])
                    last_ret = [operand_1[0], list_result]
                    return [operand_1[0], list_result]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_1[0] == 'element':
                    list_result = cell_XOR(operand_1[1], operand_2[1])
                    last_ret = [operand_1[0], list_result]
                    return [operand_1[0], list_result]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'matrix_Sgn' or operand_1[0] == 'matrix_int':
                if operand_2[0] == 'matrix_Sgn' or operand_2[0] == 'matrix_UnSgn' or operand_2[0] == 'matrix_int':
                    if len(operand_1[1]) != len(operand_2[1]) or len(operand_1[1][0]) != len(operand_2[1][0]):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                matrix_result[1][i][j][1] -= operand_2[1][i][j][1]
                        last_ret = matrix_result
                        return matrix_result
                elif operand_2[0] == 'matrix_Cell' or operand_2[0] == 'matrix_element':
                    if len(operand_1[1]) != len(operand_2[1]) or len(operand_1[1][0]) != len(operand_2[1][0]):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                tmp = operand_2[1][i][j]
                                tmp = cast_Cell_SgnUnSgn(tmp)
                                matrix_result[1][i][j][1] -= tmp[1]
                        last_ret = matrix_result
                        return matrix_result
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'matrix_UnSgn':
                if operand_2[0] == 'matrix_Sgn' or operand_2[0] == 'matrix_int':
                    if len(operand_1[1]) != len(operand_2[1]) or len(operand_1[1][0]) != len(operand_2[1][0]):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                tmp = operand_2[1][i][j]
                                tmp = cast_integerVar_UnSgn(tmp)
                                if ml_errors:
                                    return
                                if matrix_result[1][i][j][1] - tmp[1] < 0:
                                    ml_errors += 1 
                                    ml_error_list.append('vihod za razryadnuyu setku')
                                    return
                                matrix_result[1][i][j][1] -= tmp[1]
                        last_ret = matrix_result
                        return matrix_result
                elif operand_2[0] == 'matrix_UnSgn':
                    if len(operand_1[1]) != len(operand_2[1]) or len(operand_1[1][0]) != len(operand_2[1][0]):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                if matrix_result[1][i][j][1] - operand_2[1][i][j][1] < 0:
                                    ml_errors += 1 
                                    ml_error_list.append('vihod za razryadnuyu setku')
                                    return
                                matrix_result[1][i][j][1] -= operand_2[1][i][j][1]
                        last_ret = matrix_result
                        return matrix_result
                elif operand_2[0] == 'matrix_Cell' or operand_2[0] == 'matrix_element':
                    if len(operand_1[1]) != len(operand_2[1]) or len(operand_1[1][0]) != len(operand_2[1][0]):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                tmp = operand_2[1][i][j]
                                tmp = cast_Cell_SgnUnSgn(tmp)
                                if matrix_result[1][i][j][1] - tmp[1] < 0:
                                    ml_errors += 1 
                                    ml_error_list.append('vihod za razryadnuyu setku')
                                    return
                                matrix_result[1][i][j][1] -= tmp[1]
                        last_ret = matrix_result
                        return matrix_result
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'matrix_Cell' or operand_1[0] == 'matrix_element':
                if operand_2[0] == 'matrix_Sgn' or operand_2[0] == 'matrix_UnSgn' or operand_2[0] == 'matrix_int':
                    if len(operand_1[1]) != len(operand_2[1]) or len(operand_1[1][0]) != len(operand_2[1][0]):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                tmp = operand_2[1][i][j]
                                tmp = cast_integerVar_Cell(tmp)
                                matrix_result[1][i][j][1] = cell_XOR(matrix_result[1][i][j][1], tmp[1])
                        last_ret = matrix_result
                        return matrix_result
                elif operand_2[0] == 'matrix_Cell' or operand_2[0] == 'matrix_element':
                    if len(operand_1[1]) != len(operand_2[1]) or len(operand_1[1][0]) != len(operand_2[1][0]):
                        ml_errors += 1
                        ml_error_list.append('nesootvetstvie razmerov matrizz')
                        return
                    else:
                        matrix_result = operand_1
                        for i in range(len(operand_1[1])):
                            for j in range(len(operand_1[1][0])):
                                matrix_result[1][i][j][1] = cell_XOR(matrix_result[1][i][j][1], operand_2[1][i][j][1])
                        last_ret = matrix_result                        
                        return matrix_result
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
        ########Operator umnojeniya############
        if self.operator == '*':
            if operand_1[0] == 'signed' or operand_1[0] == 'const signed' or operand_1[0] == 'int':
                if operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned' or operand_2[0] == 'int':
                    last_ret = [operand_1[0], operand_1[1] * operand_2[1]]
                    return [operand_1[0], operand_1[1] * operand_2[1]]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_2[0] == 'element':
                    tmp = copy.copy(operand_2)
                    tmp = cast_Cell_SgnUnSgn(tmp)
                    last_ret = [operand_1[0], operand_1[1] * tmp[1]]
                    return [operand_1[0], operand_1[1] * tmp[1]]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'unsigned' or operand_1[0] == 'const unsigned':
                if operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned':
                    last_ret = [operand_1[0], operand_1[1] * operand_2[1]]
                    return [operand_1[0], operand_1[1] * operand_2[1]]
                elif operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'int': 
                    tmp = copy.deepcopy(operand_2)
                    tmp = cast_integerVar_UnSgn(operand_2)
                    if ml_errors:
                        return
                    else:
                        last_ret = [operand_1[0], operand_1[1] * tmp[1]]
                        return [operand_1[0], operand_1[1] * tmp[1]]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_2[0] == 'element':
                    tmp = copy.copy(operand_2)
                    tmp = cast_Cell_SgnUnSgn(tmp)
                    last_ret = [operand_1[0], operand_1[1] * tmp[1]]
                    return [operand_1[0], operand_1[1] * tmp[1]]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'cell' or operand_1[0] == 'const cell' or operand_1[0] == 'element':
                if operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned' or operand_2[0] == 'int':
                    tmp = copy.copy(operand_2)
                    tmp = cast_integerVar_Cell(tmp)
                    list_result = cell_AND(operand_1[1], tmp[1])
                    last_ret = [operand_1[0], list_result]
                    return [operand_1[0], list_result]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_1[0] == 'element':
                    list_result = cell_AND(operand_1[1], operand_2[1])
                    last_ret = [operand_1[0], list_result]
                    return [operand_1[0], list_result]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'matrix_Sgn' or operand_1[0] == 'matrix_int':
                if operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned' or operand_2[0] == 'int':
                    matrix_result = operand_1
                    for i in range(len(operand_1[1])):
                        for j in range(len(operand_1[1][0])):
                            matrix_result[1][i][j][1] = matrix_result[1][i][j][1] * operand_2[1]
                    last_ret = matrix_result
                    return matrix_result
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_2[0] == 'element':
                    tmp = copy.copy(operand_2)
                    tmp = cast_Cell_SgnUnSgn(tmp)                    
                    matrix_result = operand_1
                    for i in range(len(operand_1[1])):
                        for j in range(len(operand_1[1][0])):
                            matrix_result[1][i][j][1] = matrix_result[1][i][j][1] * tmp[1]
                    last_ret = matrix_result
                    return matrix_result
                elif operand_2[0] == 'matrix_Sgn' or operand_2[0] == 'matrix_UnSgn' or operand_2[0] == 'matrix_int':
                    if len(operand_2[1]) != len(operand_1[1][0]):
                        ml_errors += 1
                        ml_error_list.append('matrizi nesoglasovani')
                        return
                    else:
                        for z in range(0, len(operand_1[1])):
                            for i in range(0, len(operand_2[1][0])):
                                for j in range(0, operand_2[1]): 
                                    summ = summ + operand_1[1][z][i][1] * operand_2[1][i][j][1]
                                tmp_matrix.append(summ)
                                summ = 0
                            tmp_matrix_result.append(tmp_matrix)
                            tmp_matrix = []
                        last_ret = [operand_1[0], tmp_matrix_result]
                        return [operand_1[0], tmp_matrix_result]
                elif operand_2[0] == 'matrix_Cell' or operand_2[0] == 'matrix_element':
                    if len(operand_2[1]) != len(operand_1[1][0]):
                        ml_errors += 1
                        ml_error_list.append('matrizi nesoglasovani')
                        return
                    else:
                        for z in range(0, len(operand_1[1])):
                            for i in range(0, len(operand_2[1][0])):
                                for j in range(0, operand_2[1]): 
                                    tmp = operand_2[1][i][j]
                                    tmp = cast_Cell_SgnUnSgn(tmp)
                                    summ = summ + operand_1[1][z][i][1] * tmp[1]
                                tmp_matrix.append(summ)
                                summ = 0
                            tmp_matrix_result.append(tmp_matrix)
                            tmp_matrix = []
                        last_ret = [operand_1[0], tmp_matrix_result]
                        return [operand_1[0], tmp_matrix_result]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'matrix_UnSgn':
                if operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'int':
                    tmp = operand_2
                    tmp = cast_integerVar_UnSgn(tmp)
                    if ml_memory:
                        return
                    matrix_result = operand_1
                    for i in range(len(operand_1[1])):
                        for j in range(len(operand_1[1][0])):
                            matrix_result[1][i][j][1] = matrix_result[1][i][j][1] * tmp[1]
                    last_ret = matrix_result
                    return matrix_result
                elif operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned':
                    matrix_result = operand_1
                    for i in range(len(operand_1[1])):
                        for j in range(len(operand_1[1][0])):
                            matrix_result[1][i][j][1] = matrix_result[1][i][j][1] * operand_2[1]
                    last_ret = matrix_result
                    return matrix_result
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_2[0] == 'element':
                    tmp = copy.copy(operand_2)
                    tmp = cast_Cell_SgnUnSgn(tmp)                    
                    matrix_result = operand_1
                    for i in range(len(operand_1[1])):
                        for j in range(len(operand_1[1][0])):
                            matrix_result[1][i][j][1] = matrix_result[1][i][j][1] * tmp[1]
                    last_ret = matrix_result
                    return matrix_result
                elif operand_2[0] == 'matrix_Sgn' or operand_2[0] == 'matrix_int':
                    if len(operand_2[1]) != len(operand_1[1][0]):
                        ml_errors += 1
                        ml_error_list.append('matrizi nesoglasovani')
                        return
                    else:
                        for z in range(0, len(operand_1[1])):
                            for i in range(0, len(operand_2[1][0])):
                                for j in range(0, operand_2[1]): 
                                    tmp = operand_2[1][i][j]
                                    tmp = cast_integerVar_UnSgn(tmp)
                                    if ml_errors:
                                        return
                                    summ = summ + operand_1[1][z][i][1] * tmp[1]
                                tmp_matrix.append(summ)
                                summ = 0
                            tmp_matrix_result.append(tmp_matrix)
                            tmp_matrix = []
                        last_ret = [operand_1[0], tmp_matrix_result]
                        return [operand_1[0], tmp_matrix_result]
                elif operand_2[0] == 'matrix_Cell' or operand_2[0] == 'matrix_element':
                    if len(operand_2[1]) != len(operand_1[1][0]):
                        ml_errors += 1
                        ml_error_list.append('matrizi nesoglasovani')
                        return
                    else:
                        for z in range(0, len(operand_1[1])):
                            for i in range(0, len(operand_2[1][0])):
                                for j in range(0, operand_2[1]): 
                                    tmp = operand_2[1][i][j]
                                    tmp = cast_Cell_SgnUnSgn(tmp)
                                    summ = summ + operand_1[1][z][i][1] * tmp[1]
                                tmp_matrix.append(summ)
                                summ = 0
                            tmp_matrix_result.append(tmp_matrix)
                            tmp_matrix = []
                        last_ret = [operand_1[0], tmp_matrix_result]
                        return [operand_1[0], tmp_matrix_result]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'matrix_Cell' or operand_1[0] == 'matrix_element':
                if operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned' or operand_2[0] == 'int':
                    tmp = operand_2
                    tmp = cast_Cell_SgnUnSgn(tmp)
                    matrix_result = operand_1
                    for i in range(len(operand_1[1])):
                        for j in range(len(operand_1[1][0])):
                            matrix_result[1][i][j][1] = cell_AND(matrix_result[1][i][j][1], tmp[1])
                    last_ret = matrix_result
                    return matrix_result
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_2[0] == 'element':
                    matrix_result = operand_1
                    for i in range(len(operand_1[1])):
                        for j in range(len(operand_1[1][0])):
                            matrix_result[1][i][j][1] = cell_AND(matrix_result[1][i][j][1] * tmp[1])
                    last_ret = matrix_result
                    return matrix_result
                elif operand_2[0] == 'matrix_Sgn' or operand_2[0] == 'matrix_UnSgn' or operand_2[0] == 'matrix_int':
                    if len(operand_2[1]) != len(operand_1[1][0]):
                        ml_errors += 1
                        ml_error_list.append('matrizi nesoglasovani')
                        return
                    else:
                        for z in range(0, len(operand_1[1])):
                            for i in range(0, len(operand_2[1][0])):
                                for j in range(0, operand_2[1]): 
                                    tmp = operand_2[1][i][j]
                                    tmp = cast_integerVar_Cell(tmp)
                                    list_result = cell_AND(list_result, cell_AND(operand_1[1][z][i][1], tmp[1]))
                                tmp_matrix.append(list_result)
                                list_result = []
                            tmp_matrix_result.append(tmp_matrix)
                            tmp_matrix = []
                        last_ret = [operand_1[0], tmp_matrix_result]
                        return [operand_1[0], tmp_matrix_result]
                elif operand_2[0] == 'matrix_Cell' or operand_2[0] == 'matrix_element':
                    if len(operand_2[1]) != len(operand_1[1][0]):
                        ml_errors += 1
                        ml_error_list.append('matrizi nesoglasovani')
                        return
                    else:
                        for z in range(0, len(operand_1[1])):
                            for i in range(0, len(operand_2[1][0])):
                                for j in range(0, operand_2[1]):
                                    list_result = cell_AND(list_result, cell_AND(operand_1[1][z][i][1], operand_2[1][i][j][1]))
                                tmp_matrix.append(list_result)
                                list_result = []
                            tmp_matrix_result.append(tmp_matrix)
                            tmp_matrix = []
                        last_ret = [operand_1[0], tmp_matrix_result]
                        return [operand_1[0], tmp_matrix_result]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
        ########Operator deleniya############
        if self.operator == '/':
            if operand_1[0] == 'signed' or operand_1[0] == 'const signed' or operand_1[0] == 'int':
                if (operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned' or operand_2[0] == 'int'):
                    if operand_2[1] == 0: 
                        ml_errors += 1
                        ml_error_list.append('delenie na 0')
                    else:
                        last_ret = [operand_1[0], operand_1[1] // operand_2[1]]
                        return [operand_1[0], operand_1[1] // operand_2[1]]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_2[0] == 'element':
                    tmp = copy.copy(operand_2)
                    tmp = cast_Cell_SgnUnSgn(tmp)
                    if tmp[1] == 0:
                        ml_errors += 1
                        ml_error_list.append('delenie na 0')
                    else:
                        last_ret = [operand_1[0], operand_1[1] // tmp[1]]
                        return [operand_1[0], operand_1[1] // tmp[1]]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'unsigned' or operand_1[0] == 'const unsigned':
                if operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned':
                    if operand_2[1] == 0:
                        ml_errors += 1
                        ml_error_list.append('delenie na 0')
                        return
                    else:
                        last_ret = [operand_1[0], operand_1[1] // operand_2[1]]
                        return [operand_1[0], operand_1[1] // operand_2[1]]
                elif operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'int': 
                    tmp = copy.copy(operand_2)
                    tmp = cast_integerVar_UnSgn(tmp)
                    if ml_errors:
                        return
                    elif tmp[1] == 0:
                        ml_errors += 1
                        ml_error_list.append('delenie na 0')
                        return
                    else:
                        last_ret = [operand_1[0], operand_1[1] // tmp[1]]
                        return [operand_1[0], operand_1[1] // tmp[1]]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_2[0] == 'element':
                    tmp = copy.copy(operand_2)
                    tmp = cast_Cell_SgnUnSgn(tmp)
                    if tmp[1] == 0:
                        ml_errors += 1
                        ml_error_list.append('delenie na 0')
                        return
                    else:
                        last_ret = [operand_1[0], operand_1[1] // tmp[1]]
                        return [operand_1[0], operand_1[1] // tmp[1]]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'cell' or operand_1[0] == 'const cell' or operand_1[0] == 'element':
                if operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned' or operand_2[0] == 'int':
                    tmp = copy.copy(operand_2)
                    tmp = cast_integerVar_Cell(tmp)
                    list_result = cell_XOR(operand_1[1], tmp[1])
                    last_ret = [operand_1[0], list_result]
                    return [operand_1[0], list_result]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_1[0] == 'element':
                    list_result = cell_XOR(operand_1[1], operand_2[1])
                    last_ret = [operand_1[0], list_result]
                    return [operand_1[0], list_result]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            else:
                ml_errors += 1
                ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                return
        ########Operator ostatok ot deleniya############
        if self.operator == '%':
            if operand_1[0] == 'signed' or operand_1[0] == 'const signed' or operand_1[0] == 'int':
                if (operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned' or operand_2[0] == 'int'):
                    if operand_2[1] == 0: 
                        ml_errors += 1
                        ml_error_list.append('delenie na 0')
                    else:
                        last_ret = [operand_1[0], operand_1[1] % operand_2[1]]
                        return [operand_1[0], operand_1[1] % operand_2[1]]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_2[0] == 'element':
                    tmp = copy.copy(operand_2)
                    tmp = cast_Cell_SgnUnSgn(tmp)
                    if tmp[1] == 0:
                        ml_errors += 1
                        ml_error_list.append('delenie na 0')
                    else:
                        last_ret = [operand_1[0], operand_1[1] % tmp[1]]
                        return [operand_1[0], operand_1[1] % tmp[1]]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'unsigned' or operand_1[0] == 'const unsigned':
                if operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned':
                    if operand_2[1] == 0:
                        ml_errors += 1
                        ml_error_list.append('delenie na 0')
                        return
                    else:
                        last_ret = [operand_1[0], operand_1[1] % operand_2[1]]
                        return [operand_1[0], operand_1[1] % operand_2[1]]
                elif operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'int': 
                    tmp = copy.copy(operand_2)
                    tmp = cast_integerVar_UnSgn(tmp)
                    if ml_errors:
                        return
                    elif tmp[1] == 0:
                        ml_errors += 1
                        ml_error_list.append('delenie na 0')
                        return
                    else:
                        last_ret = [operand_1[0], operand_1[1] % tmp[1]]
                        return [operand_1[0], operand_1[1] % tmp[1]]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_2[0] == 'element':
                    tmp = copy.copy(operand_2)
                    tmp = cast_Cell_SgnUnSgn(tmp)
                    if tmp[1] == 0:
                        ml_errors += 1
                        ml_error_list.append('delenie na 0')
                        return
                    else:
                        last_ret = [operand_1[0], operand_1[1] % tmp[1]]
                        return [operand_1[0], operand_1[1] % tmp[1]]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            elif operand_1[0] == 'cell' or operand_1[0] == 'const cell' or operand_1[0] == 'element':
                if operand_2[0] == 'signed' or operand_2[0] == 'const signed' or operand_2[0] == 'unsigned' or operand_2[0] == 'const unsigned' or operand_2[0] == 'int':
                    tmp = copy.copy(operand_2)
                    tmp = cast_integerVar_Cell(tmp)
                    list_result = cell_XOR(operand_1[1], tmp[1])
                    last_ret = [operand_1[0], list_result]
                    return [operand_1[0], list_result]
                elif operand_2[0] == 'cell' or operand_2[0] == 'const cell' or operand_1[0] == 'element':
                    list_result = cell_XOR(operand_1[1], operand_2[1])
                    last_ret = [operand_1[0], list_result]
                    return [operand_1[0], list_result]
                else:
                    ml_errors += 1
                    ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                    return
            else:
                ml_errors += 1
                ml_error_list.append('nevozmojno proizvesti vichisleniya s dannimi tipami')
                return
        ########Operator bolshe############
        if self.operator == '>':
            if operand_1[0] == 'matrix_Sgn' or operand_2[0] == 'matrix_Sgn':
	            ml_errors += 1
	            ml_error_list.append('Bin oper "'+ self.operator+'" ne opredelen dlya dannih typov')
	            return
            if operand_1[0] == 'matrix_UnSgn' or operand_2[0] == 'matrix_UnSgn':
	            ml_errors += 1
	            ml_error_list.append('Bin oper "'+self.operator+'" ne opredelen dlya dannih typov')
	            return
            if operand_1[0] == 'matrix_Cell' or operand_2[0] == 'matrix_Cell':
	            ml_errors += 1
	            ml_error_list.append('Bin oper "'+self.operator+'" ne opredelen dlya dannih typov')
	            return
            if operand_1[0] == 'matrix_int' or operand_2[0] == 'matrix_int':
	            ml_errors += 1
	            ml_error_list.append('Bin oper "'+self.operator+'" ne opredelen dlya dannih typov')
	            return
            if operand_1[0] == 'matrix_element' or operand_2[0] == 'matrix_element':
	            ml_errors += 1
	            ml_error_list.append('Bin oper "'+self.operator+'" ne opredelen dlya dannih typov')
	            return
            else:
	            result = 1 if operand_1[1] > operand_2[1] else 0
	            return result
        ########Operator menshe############
        if self.operator == '<':
            if operand_1[0] == 'matrix_Sgn' or operand_2[0] == 'matrix_Sgn':
	            ml_errors += 1
	            ml_error_list.append('Bin oper "'+self.operator+'" ne opredelen dlya dannih typov')
	            return
            if operand_1[0] == 'matrix_UnSgn' or operand_2[0] == 'matrix_UnSgn':
	            ml_errors += 1
	            ml_error_list.append('Bin oper "'+self.operator+'" ne opredelen dlya dannih typov')
	            return
            if operand_1[0] == 'matrix_Cell' or operand_2[0] == 'matrix_Cell':
	            ml_errors += 1
	            ml_error_list.append('Bin oper "'+self.operator+'" ne opredelen dlya dannih typov')
	            return
            if operand_1[0] == 'matrix_int' or operand_2[0] == 'matrix_int':
	            ml_errors += 1
	            ml_error_list.append('Bin oper "'+self.operator+'" ne opredelen dlya dannih typov')
	            return
            if operand_1[0] == 'matrix_element' or operand_2[0] == 'matrix_element':
	            ml_errors += 1
	            ml_error_list.append('Bin oper "'+self.operator+'" ne opredelen dlya dannih typov')
	            return
            else:
	            result = 1 if operand_1[1] < operand_2[1] else 0
	            return result
        ########Operator ravno############
        if self.operator == '=':
            result = 0 if operand_1[1] != operand_2[1] else 1
            #print("RAVNO " + str(operand_1) + " " + str(operand_2) + " " + str(result))
            return result

def cast_Cell_SgnUnSgn(argument):
    result = copy.deepcopy(argument)    
    result[0] = 'signed'
    if (result[1][0] == 'top') or (result[1][1] == 'right') or(result[1][2] == 'down') or (result[1][3] == 'left'):
        result[1] = 1
    else:
        result[1] = 0
    return result

def cast_integerVar_UnSgn(argument):
    global ml_errors
    global ml_error_list
    result = copy.deepcopy(argument)
    result[0] = 'signed'
    if result[1] < 0:
        ml_errors += 1
        ml_error_list.append('vihod za razryadnuyu setku')
        return
    else:
        result[1] = int(result[1])
    return result

def cast_integerVar_Cell(argument):
    result = copy.deepcopy(argument)
    result[0] = 'cell'
    if result[1] == 0:
        list_result = ['ntop', 'nright', 'ndown', 'nleft']
    else:
        list_result = ['top', 'right', 'down', 'left']
    result[1] = list_result
    return result

def cell_AND(operand_1, operand_2):
    list_result = []
    for i in range(0, 4):
        if operand_1[i] == 'top' and operand_2[i] == 'top':
	        list_result.append('top')
        elif operand_1[i] == 'top' and operand_2[i] == 'ntop':
            list_result.append('ntop')
        elif operand_1[i] == 'ntop' and operand_2[i] == 'top':
            list_result.append('ntop')
        elif operand_1[i] == 'ntop' and operand_2[i] == 'ntop':
	        list_result.append('ntop')
        if operand_1[i] == 'right' and operand_2[i] == 'right':
	        list_result.append('right')
        elif operand_1[i] == 'right' and operand_2[i] == 'nright':
	        list_result.append('nright')
        elif operand_1[i] == 'nright' and operand_2[i] == 'right':
	        list_result.append('nright')
        elif operand_1[i] == 'nright' and operand_2[i] == 'nright':
	        list_result.append('nright')
        if operand_1[i] == 'down' and operand_2[i] == 'down':
	        list_result.append('down')
        elif operand_1[i] == 'down' and operand_2[i] == 'ndown':
	        list_result.append('ndown')
        elif operand_1[i] == 'ndown' and operand_2[i] == 'down':
	        list_result.append('ndown')
        elif operand_1[i] == 'ndown' and operand_2[i] == 'ndown':
	        list_result.append('ndown')
        if operand_1[i] == 'left' and operand_2[i] == 'left':
	        list_result.append('left')
        elif operand_1[i] == 'left' and operand_2[i] == 'nleft':
	        list_result.append('nleft')
        elif operand_1[i] == 'nleft' and operand_2[i] == 'left':
	        list_result.append('nleft')
        elif operand_1[i] == 'nleft' and operand_2[i] == 'nleft':
	        list_result.append('nleft')
    return list_result

def cell_OR(operand_1, operand_2):
    list_result = []
    for i in range(0, 4):
        if operand_1[i] == 'top' and operand_2[i] == 'top':
            list_result.append('top')
        elif operand_1[i] == 'top' and operand_2[i] == 'ntop':
            list_result.append('top')
        elif operand_1[i] == 'ntop' and operand_2[i] == 'top':
            list_result.append('top')
        elif operand_1[i] == 'ntop' and operand_2[i] == 'ntop':
            list_result.append('ntop')
        if operand_1[i] == 'right' and operand_2[i] == 'right':
            list_result.append('right')
        elif operand_1[i] == 'right' and operand_2[i] == 'nright':
            list_result.append('right')
        elif operand_1[i] == 'nright' and operand_2[i] == 'right':
            list_result.append('right')
        elif operand_1[i] == 'nright' and operand_2[i] == 'nright':
            list_result.append('nright')
        if operand_1[i] == 'down' and operand_2[i] == 'down':
            list_result.append('down')
        elif operand_1[i] == 'down' and operand_2[i] == 'ndown':
            list_result.append('down')
        elif operand_1[i] == 'ndown' and operand_2[i] == 'down':
            list_result.append('down')
        elif operand_1[i] == 'ndown' and operand_2[i] == 'ndown':
            list_result.append('ndown')
        if operand_1[i] == 'left' and operand_2[i] == 'left':
            list_result.append('left')
        elif operand_1[i] == 'left' and operand_2[i] == 'nleft':
            list_result.append('left')
        elif operand_1[i] == 'nleft' and operand_2[i] == 'left':
            list_result.append('left')
        elif operand_1[i] == 'nleft' and operand_2[i] == 'nleft':
            list_result.append('nleft')
    return list_result

def cell_XOR(operand_1, operand_2):
    list_result = []
    for i in range(0, 4):
        if operand_1[i] == 'top' and operand_2[i] == 'top':
            list_result.append('ntop')
        elif operand_1[i] == 'top' and operand_2[i] == 'ntop':
            list_result.append('top')
        elif operand_1[i] == 'ntop' and operand_2[i] == 'top':
            list_result.append('top')
        elif operand_1[i] == 'ntop' and operand_2[i] == 'ntop':
            list_result.append('ntop')
        if operand_1[i] == 'right' and operand_2[i] == 'right':
            list_result.append('nright')
        elif operand_1[i] == 'right' and operand_2[i] == 'nright':
            list_result.append('right')
        elif operand_1[i] == 'nright' and operand_2[i] == 'right':
            list_result.append('right')
        elif operand_1[i] == 'nright' and operand_2[i] == 'nright':
            list_result.append('nright')
        if operand_1[i] == 'down' and operand_2[i] == 'down':
            list_result.append('ndown')
        elif operand_1[i] == 'down' and operand_2[i] == 'ndown':
            list_result.append('down')
        elif operand_1[i] == 'ndown' and operand_2[i] == 'down':
            list_result.append('down')
        elif operand_1[i] == 'ndown' and operand_2[i] == 'ndown':
            list_result.append('ndown')
        if operand_1[i] == 'left' and operand_2[i] == 'left':
            list_result.append('nleft')
        elif operand_1[i] == 'left' and operand_2[i] == 'nleft':
            list_result.append('left')
        elif operand_1[i] == 'nleft' and operand_2[i] == 'left':
            list_result.append('left')
        elif operand_1[i] == 'nleft' and operand_2[i] == 'nleft':
            list_result.append('nleft')
    return list_result

class testonce:
    def __init__(self, testonceArg, condition, statementGroup):
        self.testonceArg = testonceArg
        self.condition = condition
        self.statementGroup = copy.deepcopy(statementGroup)
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        global last_ret
        if ml_errors:
	        return
        condition = self.condition.visit(key)
        if condition != 0:
            if isinstance(self.statementGroup, list):
                for i in self.statementGroup:
                    i.visit(key)
                    if ml_errors:
                        return
            else:
                self.statementGroup.visit(key)

class testrep:
    def __init__(self, testrepArg, condition, statementGroup):
        self.testrepArg = testrepArg
        self.condition = condition
        self.statementGroup = copy.deepcopy(statementGroup)
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        if ml_errors:
	        return
        while self.condition.visit(key) != 0:
	        if isinstance(self.statementGroup, list):
		        for i in self.statementGroup:
			        i.visit(key)
			        if ml_errors:
				        return
	        else:
		        self.statementGroup.visit(key)
		        if ml_errors:
			        return

class cell:
    def __init__(self, var_1, var_2, var_3, var_4):
        self.var_1 = var_1
        self.var_2 = var_2
        self.var_3 = var_3
        self.var_4 = var_4
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        if ml_errors:
	        return
        return ['element', [self.var_1, self.var_2, self.var_3, self.var_4]]

class matrixOneMore:
    def __init__(self, list_result):
        self.list_result = copy.deepcopy(list_result)
    def visit(self, key):
        list_var = []
        tmp_list = []
        tmp = None
        for i in self.list_result:
	        for j in i:
		        j = j.visit(key)
		        tmp_list.append(j)
	        i = tmp_list
	        list_var.append(copy.deepcopy(i))
	        tmp_list.clear()
        if list_var[0][0][0] == 'int':
	        tmp = 'matrix_int'
        elif list_var[0][0][0] == 'cell':
	        tmp = 'matrix_element'
        return [tmp, list_var]

class matrixFirst:
    def __init__(self, declare, var_1, var_2):
        self.declare = declare
        self.var_1 = var_1
        self.var_2 = var_2
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        global last_declaration_var
        tmp = []
        self.declare.visit(key)
        first = self.var_1.visit(key)
        second = self.var_2.visit(key)
        if first[1] < 0 or second[1] < 0:
            ml_errors += 1
            ml_error_list.append('NEGATIVE INDEX ERROR')
            return
        if ml_errors:
            return
        if ml_memory[key][last_declaration_var][0] == 'signed' or ml_memory[key][last_declaration_var][0] == 'int':
            tmp = 'matrix_Sgn'
        elif ml_memory[key][last_declaration_var][0] == 'unsigned':
            tmp = 'matrix_UnSgn'
        elif ml_memory[key][last_declaration_var][0] == 'cell' or ml_memory[key][last_declaration_var][0] == 'element':
            tmp = 'matrix_Cell'
        ml_memory[key][last_declaration_var][0] = tmp
        ml_memory[key][last_declaration_var][1] = []
        for i in range(first[1] + 1):
            ml_memory[key][last_declaration_var][1].append([0] * (second[1] + 1))

class matrixSecond:
    def __init__(self, declare):
        self.declare = declare
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        global last_declaration_var
        tmp = []
        self.declare.visit(key)
        if ml_errors:
	        return
        if ml_memory[key][last_declaration_var][0] == 'signed' or ml_memory[key][last_declaration_var][0] == 'int':
            tmp = 'matrix_Sgn'
        elif ml_memory[key][last_declaration_var][0] == 'unsigned':
            tmp = 'matrix_UnSgn'
        elif ml_memory[key][last_declaration_var][0] == 'cell' or ml_memory[key][last_declaration_var][0] == 'element':
            tmp = 'matrix_Cell'
        ml_memory[key][last_declaration_var][0] = tmp
        ml_memory[key][last_declaration_var][1] = []

class Top:
    def __init__(self, action):
        self.action = action
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        if not ml_memory[key].get('maze'):
            ml_errors += 1
            ml_error_list.append('maze not found')
            return
        if not ml_memory[key].get('robot'):
            ml_errors += 1
            ml_error_list.append('NO ROBOT POSITION')
            return
        else:
            x = ml_memory[key]['robot'][1][1][0][1]
            y = ml_memory[key]['robot'][1][0][0][1]            
            max_x = len(ml_memory[key]['maze'][1][0])#Stolbci
            max_y = len(ml_memory[key]['maze'][1])#Stroki
            if (y == 0) and ml_memory[key]['maze'][1][y][x][1][0] == 'ntop':
	            print('VI POKINULI LABIRINT!')
	            ml_memory[key]['robot'][1][0][0][1] = 100
	            ml_memory[key]['robot'][1][1][0][1] = 100
            elif (y == 0) and ml_memory[key]['maze'][1][y][x][1][0] == 'top':
	            ml_errors += 1 
	            ml_error_list.append('robot razbilsya, TAK KAK POSHEL V STENU!!!')
	            return
            elif ml_memory[key]['maze'][1][y][x][1][0] == 'top' or ml_memory[key]['maze'][1][y - 1][x][1][2] == 'down':
	            ml_errors += 1 
	            ml_error_list.append('robot razbilsya, TAK KAK POSHEL V STENU!!!')
	            return
            else:
	            y -= 1
	            ml_memory[key]['robot'][1][0][0][1] -= 1
	            print("TOP" + str(ml_memory[key]['robot']))
	            print(ml_memory[key]['maze'][1][y][x][1])

class Right:
    def __init__(self, action):
        self.action = action
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        if not ml_memory[key].get('maze'):
	        ml_errors += 1
	        ml_error_list.append('NO LABIRINTH')
	        return
        if not ml_memory[key].get('robot'):
	        ml_errors += 1
	        ml_error_list.append('NO ROBOT POSITION')
	        return
        else:
            x = ml_memory[key]['robot'][1][1][0][1]
            y = ml_memory[key]['robot'][1][0][0][1]     
            max_x = len(ml_memory[key]['maze'][1][0])#Stolbci
            max_y = len(ml_memory[key]['maze'][1])#Stroki
            if (x == max_x - 1) and ml_memory[key]['maze'][1][y][x][1][1] == 'nright':
                print('VI POKINULI LABIRINT!')
                ml_memory[key]['robot'][1][0][0][1] = 100
                ml_memory[key]['robot'][1][1][0][1] = 100
            elif (x == max_x - 1) and ml_memory[key]['maze'][1][y][x][1][1] == 'right':
                ml_errors += 1 
                ml_error_list.append('robot razbilsya, TAK KAK POSHEL V STENU!!!')
                return
            elif ml_memory[key]['maze'][1][y][x][1][1] == 'right' or ml_memory[key]['maze'][1][y][x + 1][1][3] == 'left':
                ml_errors += 1 
                ml_error_list.append('robot razbilsya, TAK KAK POSHEL V STENU!!!q')
                return
            else:
                x += 1
                ml_memory[key]['robot'][1][1][0][1] += 1
                print("RIGHT" + str(ml_memory[key]['robot']))
                print(ml_memory[key]['maze'][1][y][x][1])

class Down:
    def __init__(self, action):
	    self.action = action
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        if not ml_memory[key].get('maze'):
	        ml_errors += 1
	        ml_error_list.append('NO LABIRINTH')
	        return
        if not ml_memory[key].get('robot'):
	        ml_errors += 1
	        ml_error_list.append('NO ROBOT POSITION')
	        return
        else:
            x = ml_memory[key]['robot'][1][1][0][1]
            y = ml_memory[key]['robot'][1][0][0][1]            
            max_x = len(ml_memory[key]['maze'][1][0])#Stolbci
            max_y = len(ml_memory[key]['maze'][1])#Stroki
            if (y == max_y) and ml_memory[key]['maze'][1][y][x][1][2] == 'ndown':
                print('VI POKINULI LABIRINT!')
                ml_memory[key]['robot'][1][0][0][1] = 100
                ml_memory[key]['robot'][1][1][0][1] = 100
            elif (y == max_y) and ml_memory[key]['maze'][1][y][x][1][2] == 'down':
                ml_errors += 1 
                ml_error_list.append('robot razbilsya, TAK KAK POSHEL V STENU!!!')
                return
            elif ml_memory[key]['maze'][1][y][x][1][3] == 'down' or ml_memory[key]['maze'][1][y + 1][x][1][2] == 'top':
                ml_errors += 1 
                ml_error_list.append('robot razbilsya, TAK KAK POSHEL V STENU!!!')
                return
            else:
                y += 1
                ml_memory[key]['robot'][1][0][0][1] += 1
                print("DOWN" + str(ml_memory[key]['robot']))
                print(ml_memory[key]['maze'][1][y][x][1])
		
class Left:
    def __init__(self, action):
	    self.action = action
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        if not ml_memory[key].get('maze'):
	        ml_errors += 1
	        ml_error_list.append('NO LABIRINTH')
	        return
        if not ml_memory[key].get('robot'):
	        ml_errors += 1
	        ml_error_list.append('NO ROBOT POSITION')
	        return
        else:
            x = ml_memory[key]['robot'][1][1][0][1]
            y = ml_memory[key]['robot'][1][0][0][1]            
            max_x = len(ml_memory[key]['maze'][1][0])#Stolbci
            max_y = len(ml_memory[key]['maze'][1])#Stroki
            if (x == 0) and ml_memory[key]['maze'][1][y][x][1][3] == 'nleft':
	            print('VI POKINULI LABIRINT!')
	            ml_memory[key]['robot'][1][0][0][1] = 100
	            ml_memory[key]['robot'][1][1][0][1] = 100
            elif (x == 0) and ml_memory[key]['maze'][1][y][x][1][3] == 'left':
	            ml_errors += 1 
	            ml_error_list.append('robot razbilsya, TAK KAK POSHEL V STENU!!!')
	            return
            elif ml_memory[key]['maze'][1][y][x][1][3] == 'left' or ml_memory[key]['maze'][1][y][x - 1][1][1] == 'right':
	            ml_errors += 1 
	            ml_error_list.append('robot razbilsya, TAK KAK POSHEL V STENU!!!')
	            return
            else:
	            x -= 1
	            ml_memory[key]['robot'][1][1][0][1] -= 1
	            print("LEFT" + str(ml_memory[key]['robot']))
	            print(ml_memory[key]['maze'][1][y][x][1])

class xray:
    def __init__(self, action):
        self.action = action
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        tmp_list = []
        if not ml_memory[key].get('maze'):
	        ml_errors += 1
	        ml_error_list.append('NO LABIRINTH')
	        return
        if not ml_memory[key].get('robot'):
	        ml_errors += 1
	        ml_error_list.append('NO ROBOT POSITION')
	        return
        if not ml_memory[key].get('vision'):
	        ml_errors += 1
	        ml_error_list.append('NO VISION')
	        return
        else:
            x = ml_memory[key]['robot'][1][1][0][1]
            y = ml_memory[key]['robot'][1][0][0][1]            
            max_x = len(ml_memory[key]['maze'][1][0])#Stolbci
            max_y = len(ml_memory[key]['maze'][1])#Stroki
            if x < 2:
                from_x = 0
            else:
                from_x = x - 2
            if max_x - 1 - x < 2:
                to_x = max_x
            else:
                to_x = x + 2
            if y < 2:
                from_y = 0
            else:
                from_y = y - 2
            if max_y - 1 - y < 2:
                to_y = max_y
            else:
                to_y = y + 2
            for i in range(from_y, to_y):
                for j in range(from_x, to_x):
                    tmp_list.append(copy.deepcopy(ml_memory[key]['maze'][1][y][x]))
                ml_memory[key]['vision'][1].append(copy.deepcopy(tmp_list))
                tmp_list.clear()
            #print(ml_memory[key]['vision'])
            return ml_memory[key]['vision']

class unop:
    def __init__(self, varID, operator):
        self.varID = varID
        self.operator = operator
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        if ml_errors:
            return
        summ = 0
        if ml_memory[key].get(self.varID):
            length_row = len(ml_memory[key][self.varID][1])
            length_column = len(ml_memory[key][self.varID][1][0])
            amount = length_row * length_column
            if amount == 0:
                ml_errors += 1
                ml_error_list.append('DELENIE NA NOL')
                return
            if ml_memory[key][self.varID][0] == 'matrix_Sgn' or ml_memory[key][self.varID][0] == 'matrix_UnSgn' or ml_memory[key][self.varID][0] == 'matrix_int':
                for i in range(length_row):
                    for j in range(length_column):
                        summ += ml_memory[key][self.varID][1][i][j][1]
                summ = summ // amount
                for i in range(length_row):
                    for j in range(length_column):
                        ml_memory[key][self.varID][1][i][j][1] = summ
                return ml_memory[key][self.varID]
            elif ml_memory[key][self.varID][0] == 'matrix_Cell' or ml_memory[key][self.varID][0] == 'matrix_element':
                for i in range(length_row):
                    for j in range(length_column):
                        if ml_memory[key][self.varID][1][i][j] != []:
                            if i != 0:
                                if ml_memory[key][self.varID][1][i][j][1][0] == 'top' and ml_memory[key][self.varID][1][i - 1][j][1][2] == 'ndown':
                                    ml_memory[key][self.varID][1][i - 1][j][1][2] = 'down'                        
                            elif j != length_column - 1:
                                if ml_memory[key][self.varID][1][i][j][1][1] == 'right' and ml_memory[key][self.varID][1][i][j + 1][1][3] == 'nleft':
                                    ml_memory[key][self.varID][1][i][j + 1][1][3] = 'left'
                            elif i != length_row - 1:
                                if ml_memory[key][self.varID][1][i][j][1][2] == 'down' and ml_memory[key][self.varID][1][i + 1][j][1][0] == 'ntop':
                                    ml_memory[key][self.varID][1][i + 1][j][1][0] = 'top'
                            elif j != 0:
                                if ml_memory[key][self.varID][1][i][j][1][3] == 'left' and ml_memory[key][self.varID][1][i][j - 1][1][1] == 'nright':
                                    ml_memory[key][self.varID][1][i][j - 1][1][1] = 'right'
                return ml_memory[key][self.varID]		
            else:
                ml_errors += 1
                ml_error_list.append('Dannuju operaciy nelza proizvesti s dannim tipom')
                return
        else:
            ml_errors += 1
            ml_error_list.append('operanda po dannomu ID ne naideno')
            return

class access:
    def __init__(self, varID, one, two, three):
        self.varID = varID
        self.one = one
        self.two = two
        self.three = three
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        global last_declaration_var
        if ml_errors:
            return
        one = self.one.visit(key)
        two = self.two.visit(key)
        if one[1] < 0 or two[1] < 0:
            ml_errors += 1
            ml_error_list.append('Nedopustimie indexi')
            return
        if self.three != None:
	        three = self.three.visit(key) 
        varID = self.varID.visit(key)
        if not ml_memory[key].get(last_declaration_var):
            ml_errors += 1
            ml_error_list.append('Ne naideno')
            return   
        if ml_memory[key][last_declaration_var][0] != 'matrix_Sgn' and ml_memory[key][last_declaration_var][0] != 'matrix_UnSgn' and ml_memory[key][last_declaration_var][0] != 'matrix_Cell' and ml_memory[key][last_declaration_var][0] != 'matrix_element' and ml_memory[key][last_declaration_var][0] != 'matrix_int': 
            ml_errors += 1
            ml_error_list.append('Nevozmojno proizvesti operaciu s dannim tipom')
            return
        if one[1] > len(ml_memory[key][last_declaration_var][1]) or two[1] > len(ml_memory[key][last_declaration_var][1][0]):
            ml_errors += 1
            ml_error_list.append('MATRIX INDEX OUT OF RANGE')
            return
        if ml_memory[key][last_declaration_var][0] == 'matrix_Cell' or ml_memory[key][last_declaration_var][0] =='matrix_element':   
            if three[1] >= 0 and three[1] < 4:
                #print(ml_memory[key][last_declaration_var][1][one[1]][two[1]][1][three[1]] + " " + str(one[1]))
                return ml_memory[key][last_declaration_var][1][one[1]][two[1]][1][three[1]]
            else:
                ml_errors += 1
                ml_error_list.append('nedopustimi index dlya CELL')
                return
        else:
            return  ml_memory[key][last_declaration_var][1][one[1]][two[1]]

class Call:
    def __init__(self, varID, argument_list):
        self.varID = copy.copy(varID)
        self.argument_list = copy.deepcopy(argument_list)
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory
        global last_declaration_var
        global last_ret
        if ml_errors:
	        return
        if not root.get(self.varID):
	        ml_errors += 1
	        ml_error_list.append('funkciya ne naidena')
	        return
        function = root[self.varID]
        new_key = len(ml_memory)
        while ml_memory.get(str(new_key)):##Vot tut vopros
	        new_key += 1
        ml_memory[str(new_key)] = {}
        returnArgument = None
        if self.argument_list != None:
            amount_argument = len(self.argument_list)
        else:
            amount_argument = 0
        if amount_argument:
	        for i in range(0, amount_argument):
		        tmp = copy.deepcopy(self.argument_list[i].visit(key))
		        function[1][i].visit(str(new_key))
		        ml_memory[str(new_key)][last_declaration_var] = tmp
		        if ml_errors:
			        return
        if isinstance(function[2], list):
	        for i in function[2]:
		        returnArgument = i.visit(str(new_key))
		        if ml_errors:
			        return
	        #ml_memory[str(new_key)].clear()
	        if returnArgument == None:
		        returnArgument = last_ret
		        return returnArgument
	        else:
		        return returnArgument
        else:
	        returnArgument = function[2].visit(str(new_key))
	        if ml_errors:
		        return
	        #ml_memory[str(new_key)].clear()
	        if returnArgument == None:
		        returnArgument = last_ret
		        return returnArgument
	        else:
		        return returnArgument

class forRobotExpression:
    def __init__(self, robot_expression):
        self.robot_expression = robot_expression
    def visit(self, key):
        global ml_errors
        global ml_error_list
        global ml_memory		
        return self.robot_expression	

with open('Prog') as fileProg:
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
    file_parse_error.write('\n'.join(parse_list_error))
if parse_errors > 0:
    print("Errors during parsing! Check \"Parser_errors.txt\"")
else:
    print("Now you can see parsed tree:")
    print(root)

if ml_error_list:
    print(ml_error_list)
if not root.get('start'):
	ml_errors += 1
	ml_error_list.append('Ne naidena funciya s imenem "start"')
else:
	key = 'start'
	ml_memory[key] = {}
	if isinstance(root[key][2], list):
		for i in root[key][2]:
			i.visit(key)
			if ml_errors:
				print(ml_error_list)
	else:
		root[key][2].visit(key)
	with open('Run.txt', 'w') as file_Run:
		for key,val in ml_memory.items():
			file_Run.write('{}:{}\n'.format(key,val))
		#print(ml_memory)
		print(len(ml_memory))
