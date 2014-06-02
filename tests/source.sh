
STATUS=255
TMPDIR=`mktemp -d`
do_test
STATUS=$?
rm -rf $TMPDIR

exit $STATUS
