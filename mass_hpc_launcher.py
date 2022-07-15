import os

name = "-HumanDS"

problems = [#"number-io",
            #  "small-or-large",
            #  "for-loop-index",
            "compare-string-lengths",
            "double-letters",
            "fuel-cost",
            "fizz-buzz",
            #  "replace-space-with-newline",
            #  "string-lengths-backwards",
            #  "last-index-of-zero",
            # "vector-average",
            "count-odds",
            "mirror-image",
            "paired-digits",
            "snow-day",
            "gcd",
            # "super-anagrams",
            # "sum-of-squares",
            "vectors-summed",
            # "x-word-lines",
            # "negative-to-zero",
            "scrabble-score",
            # "checksum",
            # "digits",
            # "grade",
            # "median",
            "smallest",
            # "syllables"
]

with open('hpc_launcher.template', 'r') as hpc_template:
    hpc_launcher_template = hpc_template.read()


for problem in problems:
    hpc_launcher = hpc_launcher_template.replace("#qsub-name#", problem + name)
    hpc_launcher = hpc_launcher.replace("#namespace#", problem)


    temp_filename = "temp_launcher.run"
    with open(temp_filename, 'w') as temp_launcher:
        temp_launcher.write(hpc_launcher)

    os.system("qsub " + temp_filename)
    os.remove(temp_filename)
