Minor release 1.1 only changes the test file. It does three tings:
1) It adds a test that fails when the traceback algorithm simply goes to the highest scoring adjacent cell without considering the 
score for the final step.
2) It eliminates any requirement to return a specific highest scoring cell when there is more than one possibility.
3) It replaces traceback tests using SW matrices that had -Infinity at the boundaries with matrices that can actually be returned by swBuildMatrix.