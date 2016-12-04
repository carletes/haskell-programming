stops = "pbtdkg"

vowels = "aeiou"

allWords = [[s1, v, s2] | s1 <- stops, v <- vowels, s2 <- stops]

allWordsP = [['p', v, s] | v <- vowels, s <- stops]

nouns = ["house", "door", "bottle", "phone"]
