#!/bin/bash
alex hollexer.x
#happy --debug -a holparser.y
happy holparser.y
ghc --make  -fprint-potential-instances holpv.hs hollexer.hs holparser.hs holcommon.hs
