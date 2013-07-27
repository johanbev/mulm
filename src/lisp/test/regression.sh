# Runs all regression tests
LW6MAC_PATH="/Applications/LispWorks 6.1/LispWorks.app/Contents/MacOS/lispworks-6-1-0-macos-universal"
SBCL_PATH=`which sbcl`
CCL_PATH=`which ccl64`

RVAL=0

if [ -f "$LW6MAC_PATH" ]; then
		echo "Running regression tests with LW"
		"$LW6MAC_PATH" -load regression-init.lisp &> /dev/null

		if [ $? -ne 0 ];
		then
				echo "Regression failed"
				RVAL=1
		else
				echo "Regression succeded"
		fi
fi

if [ -f "$SBCL_PATH" ]; then
		echo "Running regression tests with SBCL"
		"$SBCL_PATH" --load regression-init.lisp &> /dev/null

		if [ $? -ne 0 ];
		then
				echo "Regression failed"
				RVAL=1
		else
				echo "Regression succeded"
		fi
fi

# if [ -f "$CCL_PATH" ]; then
# 		"$CCL_PATH" --load regression-init.lisp &> /dev/null

# 		if [ $? -ne 0 ];
# 		then
# 				echo "Regression failed"
# 				RVAL=1
# 		else
# 				echo "Regression succeded"
# 		fi
# fi

exit $RVAL
