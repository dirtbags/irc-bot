#!/usr/bin/env python

import re
import sys
import random

if __name__ == '__main__':
    roll = sys.argv[1]
    m = re.match('^(?P<rolls>\d+)d(?P<sides>\d+)(x(?P<multiplier>\d+))?$', roll)
    if m:
        rolls = int(m.group('rolls'))
        sides = int(m.group('sides'))
        if m.group('multiplier'):
            multiplier = int(m.group('multiplier'))
        else:
            multiplier = 1

        dice = []
        acc = 0
        for i in range(rolls):
            n = random.randint(1, sides)
            dice.append(n)
            acc += n
        acc *= multiplier
        if rolls > 1:
            print '%s: %d %r' % (roll, acc, dice)
        else:
            print '%s: %d' % (roll, acc)

