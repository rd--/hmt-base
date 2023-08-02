cmd = db gl obj # ply
cmd_prefix = hmt

include ~/sw/hsc3-meta/mk/cmd.mk

mk-gl:
	(make gl ; cp gl $(prefix)/bin/hmt-gl)

mk-gr-planar:
	(g++ -Wall -g gr-planar.cpp -o gr-planar ; mv gr-planar $(prefix)/bin/hmt-gr-planar)

mk-ply:
	(make ply ; cp ply $(prefix)/bin/hmt-ply)

mk-dep:
	cabal v1-install --allow-newer ply-loader
