#!/bin/sh

stack install

RESOLVER=$(grep -i "resolver:" stack.yaml | sed -e "s/.*: *//")

CFGDIR=$HOME/.config/hwk
CFGFILE=$CFGDIR/Hwk.hs

if [ -f "$CFGFILE" ]; then
    mv $CFGFILE $CFGFILE.backup
elif [ ! -d "$CFGDIR" ]; then
    mkdir -p $CFGDIR
fi

cp data/Hwk.hs $CFGDIR

mv -f ~/.local/bin/hwk ~/.local/bin/hwk-bin

cat > ~/.local/bin/hwk <<EOF
#!/bin/sh

stack --resolver $RESOLVER exec ~/.local/bin/hwk-bin "\$@"
EOF

chmod u+x ~/.local/bin/hwk
