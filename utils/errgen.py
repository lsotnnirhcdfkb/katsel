#!/usr/bin/env python3

UNDER0 = ('^')
UNDER1 = ('=')
UNDER2 = ('~')
UNDER3 = ('-')

regions = {
    (0, 100): 'lexing errors',
}

errors = [
    {
        'code': 1,
        'name': 'unexpected-char',
        'enum': 'UNEXPECTEDCHAR',
        'inputs': 'Token const &tok',
        'highlights', [
            ('tok', UNDERS[0], ('error', 'unexpected character'))
        ]
    },
]
