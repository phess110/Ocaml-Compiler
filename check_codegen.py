#!/bin/env python3
# Princeton Ferro
# parser for assignment 4 codegen
# October 18, 2018
import sys
from parser import *

grammar = [\
        ('program', [['decls_list']]),\
        ('decls_list', [['decl', 'decls_list'], []]),\
        ('decl', [['type_ident', 'decl_rest'], [';']]),\
        ('decl_rest', [['global', '[', 'number', ']', ';'], ['ident', '(', 'param_list', ')', 'func_decl_rest']]),\
        ('func_decl_rest', [['{', 'stmt_list', '}'], [';']]),\
        ('param', [['type_ident', 'ident']]),\
        ('param_list', [['void'], ['param', 'param_list_cont'], []]),\
        ('param_list_cont', [[',', 'param', 'param_list'], []]),\
        ('stmt_list', [['stmt', 'stmt_list'], []]),\
        ('stmt', [['read', '(', 'ref', ')', ';'], ['write', '(', 'ref', ')', ';'], ['printf', '(', 'string', 'printf_ref_list', ')', ';'], ['return', 'return_stmt_rest', ';'], ['goto_inst'], ['assignment'], ['label_inst_or_func_call'], ['cond_goto'], ['int', 'local', '[', 'number', ']', ';'], [';']]),\
        ('printf_ref_list', [[',', 'ref', 'printf_ref_list'], []]),\
        ('return_stmt_rest', [['ref'], ['ident'], ['number'], []]),\
        ('assignment', [['ref', '=', 'operand', 'assignment_tail']]),\
        ('assignment_tail', [[';'], ['binary_operation', 'operand', ';']]),\
        ('binary_operation', [['+'], ['-'], ['*'], ['/'], ['%'], ['comparison_op']]),\
        ('ref', [['local', '[', 'index', ']'], ['global', '[', 'index', ']']]),\
        ('index', [['number'], ['local', '[', 'number', ']']]),\
        ('operand', [['number'], ['ref'], ['func_call_or_ident']]),\
        ('func_call_or_ident', [['ident', 'func_call_rest']]),\
        ('func_call_rest', [['(', 'operand_list', ')'], []]),\
        ('operand_list', [['operand', 'operand_list_cont'], []]),\
        ('operand_list_cont', [[',', 'operand', 'operand_list_cont'], []]),\
        ('label_inst_or_func_call', [['ident', 'label_inst_or_func_call_tail', ';']]),\
        ('label_inst_or_func_call_tail', [[':'], ['(', 'operand_list', ')']]),\
        ('goto_inst', [['goto', 'ident', ';']]),\
        ('cond_goto', [['if', '(', 'operand', 'comparison_op', 'operand', ')', 'goto_inst']]),\
        ('comparison_op', [['<='], ['<'], ['>='], ['>'], ['=='], ['&&'], ['||'], ['!=']]),\
        ]


if __name__ == "__main__":
    import sys
    from argparse import *

    aparser = ArgumentParser()
    aparser.add_argument('c_file')
    aparser.add_argument('-v', '--verbose', action='store_true', help='Print all messages')
    aparser.add_argument('-s', '--debug-scanner', action='store_true', help='Debug only scanner')
    aparser.add_argument('-p', '--debug-parser', action='store_true', help='Debug only parser')

    try:
        args = aparser.parse_args()
        f = sys.stdin if args.c_file == '-' else open(args.c_file, 'rt')
        tk = Tokenizer(f, debug=args.verbose or args.debug_scanner)
        tree = Parser(tk, 'program', grammar, debug=args.verbose or args.debug_parser).parse()
        print('passed')
        f.close()
    except ParseError as e:
        sys.exit(f'syntax error: {e}')
