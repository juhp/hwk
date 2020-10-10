#!/bin/sh

set -e

stack install

RESOLVER=$(grep -i "resolver:" stack.yaml | sed -e "s/.*: *//")

CFGDIR=$HOME/.config/hwk
mkdir -p $CFGDIR

if [ -f "$CFGDIR/Hwk.hs" ]; then
    CFGHASH=$(cd $CFGDIR; md5sum Hwk.hs)
fi
if [ "$CFGHASH" != "$(cd data; md5sum Hwk.hs)" ]; then
cp -p -v --backup=numbered data/Hwk.hs $CFGDIR/
fi

mv -f ~/.local/bin/hwk ~/.local/bin/hwk-bin

cat > ~/.local/bin/hwk <<EOF
#!/bin/sh

stack --resolver $RESOLVER exec ~/.local/bin/hwk-bin "\$@"
EOF

chmod u+x ~/.local/bin/hwk
