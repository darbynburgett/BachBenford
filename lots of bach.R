### The final and best program
### Epical
### Input MIDI files from a folder. Spit chi pvalue, delta, corr

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

# Set directory
# setwd("C:\\Users\\fluff\\Downloads\\Summer Research 22\\Bach Sonatas")

# Defines information for Benford
digit <-c(1,2,3,4,5,6,7,8,9)
freqBenford <-log10((digit+1)/digit)
testResults <- data.frame(1,2,3)

# Create a list of files from the target directory
songlist <- list.files(path=setwd("C:\\Users\\fluff\\Downloads\\Summer Research 22\\Bach Sonatas"),pattern=".mid")
  
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
  leadingTabulated <- tabulate(leadingPitchVector)
  freqLeadingPitch <- leadingTabulated/sum(leadingTabulated)

  # correlation tests to compare leading to benford
  chisq_p <- chisq.test(freqLeadingPitch, p = freqBenford)[[3]]
  delta <- 100*max(abs(freqLeadingPitch - freqBenford))
  Cor_t <- cor.test(freqLeadingPitch, freqBenford)[[4]]
output <- c(chisq_p,delta,Cor_t)
output
}

Output <- matrix(0,nrow=length(songlist),ncol=3)

for (i in 1:length(songlist)){
Output[i,] <- Myprogram(songlist[i])

}

