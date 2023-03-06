import os

name = "-HumanDS" ## Human Driven Simulation

problems = [
            ### PSB1
            #"software.number-io",
            #  "software.small-or-large",
            #  "software.for-loop-index",
            # "software.last-index-of-zero", ### TMH: Not adding zeros to added cases for some reason
            "software.compare-string-lengths", #DONE
            "software.double-letters", #DONE-printing checked
            "software.x-word-lines", #DONE-printing checked
            "software.negative-to-zero", #DONE
            "software.scrabble-score", #DONE
            "software.count-odds", #DONE
            #  "software.replace-space-with-newline",
            #  "software.string-lengths-backwards",
            # "software.vector-average",
            # "software.mirror-image",
            # "software.super-anagrams",
            # "software.sum-of-squares",
            # "software.vectors-summed",
            # "software.checksum",
            # "software.digits",
            # "software.grade",
            # "software.median",
            # "software.smallest",
            # "software.syllables"

            ### PSB2
            "psb2.fuel-cost", #DONE
            "psb2.fizz-buzz", #DONE
            "psb2.gcd", #DONE
            "psb2.paired-digits", #DONE
            "psb2.snow-day", #DONE
            "psb2.camel-case", #DONE
            # "psb2.find-pair", ## Broken -- haven't figured out how to handle multiple outputs
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
