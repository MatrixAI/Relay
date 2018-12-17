import sys 

'''
Usage: python3 conn_uniq.py conns_path
'''

conns_path = sys.argv[1]

conns = {}

# get only unique connections as "IF1 IF2" strings and store them in a
# dictionary
with open(conns_path, 'r') as f:
    for line in f:
        line = line.strip()
        ends = line.split(' ')

        flip = ends[1]+' '+ends[0]
        norm = ends[0]+' '+ends[1]
        
        if norm in conns:
            continue
        if flip in conns:
            continue
        conns[norm] = ends[2]

# overwrite old file
with open(conns_path+'.uniq', 'w') as f:
    for line in conns:
        f.write(line+' '+conns[line]+'\n')
