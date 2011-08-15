# WARNING this script will delete existing deliverables and models

LW6MAC_PATH="/Applications/LispWorks 6.0/LispWorks.app/Contents/MacOS/lispworks-6-0-0-macos-universal"
SBCL_PATH=`which sbcl`
RVAL=0

if [ -f ../../../eval/wsj/wsj.tt.mulm ]; then
		echo "Removing old model"
		rm ../../../eval/wsj/wsj.tt.mulm
		echo "Removing old deliveries"

		if [ -f ../mulm-sbcl-test ]; then rm ../mulm-sbcl-test; fi
		if [ -f mulm-sbcl-test ]; then rm mulm-sbcl-test; fi
		if [ -f ../mulm-lw-test ]; then rm ../mulm-lw-test; fi
		if [ -f mulm-lw-test ]; then rm mulm-lw-test; fi
				
fi

if [ -f "$LW6MAC_PATH" ]; then
		echo "Rebuilding LW deliverable"
		cd ..
		"$LW6MAC_PATH" -build delivery-lw.lisp &> /dev/null
		mv mulm-lw-test test/mulm-lw-test
		cd test
		echo "Training model"
		./mulm-lw-test train ../../../eval/wsj/wsj.tt
		echo "Tagging"
		./mulm-lw-test tag ../../../eval/wsj/wsj.tt.mulm ../../../eval/wsj/test.in > test.out.lw

		diff test.out.lw test.regression.target &> /dev/null

		if [ $? -ne 0 ];
		then
				echo "Regression failed"
				rm ../../../eval/wsj/wsj.tt.mulm mulm-lw-test
				RVAL=1
		else
				echo "Regression succeded"
				rm ../../../eval/wsj/wsj.tt.mulm test.out.lw mulm-lw-test
		fi
fi

if [ -f "$SBCL_PATH" ]; then
		echo "Rebuilding SBCL deliverable"
		cd ..
		"$SBCL_PATH" --load delivery-sbcl.lisp &> /dev/null
		mv mulm-sbcl-test test/mulm-sbcl-test
		cd test
		echo "Training model"
		./mulm-sbcl-test train ../../../eval/wsj/wsj.tt
		echo "Tagging"
		./mulm-sbcl-test tag ../../../eval/wsj/wsj.tt.mulm ../../../eval/wsj/test.in > test.out.sbcl

		diff test.out.sbcl test.regression.target &> /dev/null

		if [ $? -ne 0 ];
		then
				echo "Regression failed"
				rm ../../../eval/wsj/wsj.tt.mulm mulm-sbcl-test
				RVAL=1
		else
				echo "Regression succeded"
				rm ../../../eval/wsj/wsj.tt.mulm test.out.sbcl mulm-sbcl-test
		fi
fi


exit $RVAL
