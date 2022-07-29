# Load Packages
library(tuneR)
library(benford.analysis)
library(stringr)


# Reads the MIDI file; if there's an error, it will let you know

readMidiError <- function(midfile) {
  tryCatch(
    {
      y = readMidi(midfile)
      return(y)

    },
    error=function(error_message) {
      message(paste("File with an error", midfile))
      message("And below is the error message from R:")
      message(error_message)
      return(NA)
    }
  )
}

# Reads the MIDI file notes, if there's an error, will let you know
getMidiError <- function(song) {
  tryCatch(
    # This is what I want to do...
    {
      y = getMidiNotes(song)
      pass <- TRUE
	return(y)
	
    },
    # ... but if an error occurs, tell me what happened: 
    error=function(error_message) {
      message(paste("File with an error", head(song)))
      message("And below is the error message from R:")
      message(error_message)
      return(NA)
    }
  )
}


# Defines information for Benford
digit <-c(1,2,3,4,5,6,7,8,9)
freqBenford <-log10((digit+1)/digit)
testResults <- data.frame(1,2,3)


setwd("C:\\Users\\fluff\\Downloads\\Summer Research 22\\Musescore\\")
# Create a list of files from the target directory
songlist <- list.files(path=setwd("C:\\Users\\fluff\\Downloads\\Summer Research 22\\Musescore"), pattern = ".mid")
 

longList <- c() 
Myprogram<-function(file){

  # Reads file, gets song object
 
  song <- readMidiError(file)

  # making df for notes
  funNotes <- getMidiError(song)
  funRowCount <- nrow(song)
  


  # converts midi note value to frequency, or pitch as i will call it
  noteToPitchCalculation <- function(note){
    freq<-440*(2^((note-69)/12))
    return(freq)
  }
  # goes through each row in notes and performs calculation. 
  pitchVector <- noteToPitchCalculation(funNotes[,5])
  leadingPitchVector <- as.numeric(substr(pitchVector, 1, 1))
  longList <- append(longList, leadingPitchVector)
  leadingTabulated <- tabulate(leadingPitchVector)
  freqLeadingPitch <- leadingTabulated/sum(leadingTabulated)

  # correlation tests to compare leading to benford
  #chisq_p <- chisq.test(freqLeadingPitch, p = freqBenford)[[3]]
  delta <- 100*max(abs(freqLeadingPitch - freqBenford))
  #Cor_t <- cor.test(freqLeadingPitch, freqBenford)[[4]]
output <- c(delta)
output

}



# Output1 <- matrix(0,nrow=30,ncol=1)

# for (i in 1:30){
# Output1[i,] <- Myprogram(songlist[i])

# }

Output2 <- matrix(0,nrow=58,ncol=1)

for (i in 1:58){
Output2[i,] <- Myprogram(songlist[i])

}

rownames(Output2)<-c("songlist")
colnames(Output2)<-"Delta"


longTabulated <- tabulate(longList)
longFreq <- longTabulated/sum(longTabulated)
longDF <- data.frame(digit, freqBenford, longFreq)
write.csv(longDF, "EverySong.csv")

png("LeadingDigits.png")
plot(digit, freqBenford)
lines(digit, freqBenford, col = "blue")
lines(digit, longFreq, col = "red")
title(main = "Leading Digits of 58 KPop Songs")
legend("topright", inset = 0.02, legend = c("Benford Distribution", "Total Tabulated Leading Digits"),
	col = c("blue", "red"), lty=1:2, cex = 0.8)
dev.off()

