# Affine Cipher Utility

Affine is written in Haskell. To build the executable from source you will need
to install GHC and cabal-install, to compile and handle build dependencies respectively.

Once you've downloaded the Affine.zip file, extract its contents and change your
working directory to Affine.

	unzip Affine.zip
	cd Affine

The easiest way to build and run Affine is with a cabal sandbox. A sandbox
makes it possible to isolate your project files from your cabal package directory.
To do this we execute:

	cabal sandbox init
	cabal install

Once cabal finishes fetching and compiling dependencies you should be able to
execute affine with:

	.cabal-sandbox/bin/affine <mode> <input> <output> (<a> <b> | <dictionary>)

You can also install affine to your main cabal package directory by omitting the
'cabal sandbox init' step.
