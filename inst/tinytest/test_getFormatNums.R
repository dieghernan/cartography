seqnums <- seq(1,50)
expect_equal(as.character(seqnums) , getFormatNums(seqnums))
expect_error(getFormatNums(c("A","B")))
expect_silent(getFormatNums(seqnums)) 
expect_silent(getFormatNums(seqnums/100, prefix = "S", suffix = "€")) 
expect_silent(getFormatNums(seqnums/100, suffix = "%", replace.zero = "AA", leadzero = FALSE)) 
expect_silent(getFormatNums(seqnums, align = "right"))

