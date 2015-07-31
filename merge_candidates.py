# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import datrie, string
import os, csv, glob
from difflib import SequenceMatcher


turkish_letters = string.ascii_letters + "ŞşİıĞğÜüÖöÇç"
trie_chars = turkish_letters + " "




def get_assoc_tries(ipath):
    f = open(ipath, "r", encoding="utf-8")
    reader = csv.DictReader(f, delimiter="\t")
    rows = [row for row in reader]
    f.close()
    
    target_names = sorted(set([row['Target'] for row in rows])) # Get target names
    
    targets = {}
    for t in target_names:
        targets[t] = datrie.Trie(trie_chars)    # Make a trie for each target

    for row in rows:
        target, assoc, n = row['Target'], row['Assoc'], int(row['n'])
        targets[target][assoc] = n # targets["sofra"]["assoc"] = n
    return targets

def get_dups(targets):
    dups = {}
    for t in targets.keys():
        d = [w for w in  [targets[t].items(k) for k in targets[t].keys()] if len(w) > 1]
        if len(d) > 0:
            dups[t] = d
    return dups

def write_dups(opath, dups):
    f = open(opath, "w", encoding="utf-8")
    writer = csv.DictWriter(f, fieldnames=['Target', 'Assoc', 'n', 'Root', 'Ratio'], delimiter='\t')
    writer.writeheader()
    for t in dups:
        for dupset in dups[t]:
            root = dupset[0][0]
            for w in dupset:
                ratio = round(SequenceMatcher(None, root, w[0]).ratio(), 2)
                row = {'Target' : t, 'Assoc' : w[0], 'n' : w[1], 'Root' : root, 'Ratio' : ratio}
                writer.writerow(row)
    f.close()


        
    

ipaths = [os.path.abspath(p) for p in glob.glob('data/frequency/*.txt')]

for ipath in ipaths:
    name = os.path.splitext(ipath)[0]
    t = get_assoc_tries(ipath)
    d = get_dups(t)
    opath = 'data/merge candidates/' + os.path.splitext(os.path.basename(ipath))[0] + '_merge_candidates' + '.txt'
    write_dups(opath, d)

