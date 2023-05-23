--Tuples
randTuple = (1, "Random Tuple")
bobSmith = ("Bob Smith", 52)
bobsNames = fst bobSmith --fst gets first value
bobsAge = snd bobSmith   --snd gets second value
names = ["Bob", "Mary", "Tom"]
addresses = ["123 Main", "234 North", "567 South"]
namesNAddress = zip names addresses --combines lists into tuples
