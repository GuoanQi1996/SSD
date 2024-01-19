# packages requirement
options(warn = -1)
necessary=c('stats','data.table','dplyr','ggplot2','Rcpp')
installed=necessary %in% installed.packages()[, 'Package']
if (length(necessary[!installed]) >=1)
  install.packages(necessary[!installed],repos='http://cran.us.r-project.org')
info=lapply(necessary, function(x){library(x,character.only = T,warn.conflicts = F,quietly = T)})
options(warn = 0)

# define functions
getopt = function (spec=NULL,opt=NULL,command=get_Rscript_filename(),usage=FALSE,debug=FALSE) {
  
  # littler compatibility - map argv vector to opt
  if (is.null(opt)) {
    if (exists("argv", where = .GlobalEnv, inherits = FALSE)) {
      opt = get("argv", envir = .GlobalEnv) #nocov
    } else {
      opt = commandArgs(TRUE)
    }
  }
  
  ncol=4
  maxcol=6
  col.long.name    = 1
  col.short.name   = 2
  col.has.argument = 3
  col.mode         = 4
  col.description  = 5
  
  flag.no.argument = 0
  flag.required.argument = 1
  flag.optional.argument = 2
  
  result = list()
  result$ARGS = vector(mode="character")
  
  #no spec.  fail.
  if ( is.null(spec) ) {
    stop('argument "spec" must be non-null.')
    
    #spec is not a matrix.  attempt to coerce, if possible.  issue a warning.
  } else if ( !is.matrix(spec) ) {
    if ( length(spec)/4 == as.integer(length(spec)/4) ) {
      warning('argument "spec" was coerced to a 4-column (row-major) matrix.  use a matrix to prevent the coercion')
      spec = matrix( spec, ncol=ncol, byrow=TRUE )
    } else {
      stop('argument "spec" must be a matrix, or a character vector with length divisible by 4, rtfm.')
    }
    
    #spec is a matrix, but it has too few columns.
  } else if ( dim(spec)[2] < ncol ) {
    stop(paste('"spec" should have at least ', ncol, ' columns.',sep=''))
    
    #spec is a matrix, but it has too many columns.
  } else if ( dim(spec)[2] > maxcol ) {
    stop(paste('"spec" should have no more than ', maxcol, ' columns.',sep=''))
    
    #spec is a matrix, and it has some optional columns.
  } else if ( dim(spec)[2] != ncol ) {
    ncol = dim(spec)[2]
  }
  
  #sanity check.  make sure long names are unique, and short names are unique.
  if ( length(unique(spec[,col.long.name])) != length(spec[,col.long.name]) ) {
    stop(paste('redundant long names for flags (column ',col.long.name,' of spec matrix).',sep=''))
  }
  if ( length(stats::na.omit(unique(spec[,col.short.name]))) != length(stats::na.omit(spec[,col.short.name])) ) {
    stop(paste('redundant short names for flags (column ',col.short.name,' of spec matrix).',sep=''))
  }
  # convert numeric type to double type
  spec[,4] <- gsub("numeric", "double", spec[,4])
  
  # if usage=TRUE, don't process opt, but generate a usage string from the data in spec
  if ( usage ) {
    ret = ''
    ret = paste(ret,"Usage: ",command,sep='')
    for ( j in 1:(dim(spec))[1] ) {
      ret = paste(ret,' [-[-',spec[j,col.long.name],'|',spec[j,col.short.name],']',sep='')
      if (spec[j,col.has.argument] == flag.no.argument) {
        ret = paste(ret,']',sep='')
      } else if (spec[j,col.has.argument] == flag.required.argument) {
        ret = paste(ret,' <',spec[j,col.mode],'>]',sep='')
      } else if (spec[j,col.has.argument] == flag.optional.argument) {
        ret = paste(ret,' [<',spec[j,col.mode],'>]]',sep='')
      }
    }
    # include usage strings
    if ( ncol >= 5 ) {
      max.long = max(apply(cbind(spec[,col.long.name]),1,function(x)length(strsplit(x,'')[[1]])))
      ret = paste(ret,"\n",sep='')
      for (j in 1:(dim(spec))[1] ) {
        ret = paste(ret,sprintf(paste("    -%s|--%-",max.long,"s    %s\n",sep=''),
                                spec[j,col.short.name],spec[j,col.long.name],spec[j,col.description]
        ),sep='')
      }
    }
    else {
      ret = paste(ret,"\n",sep='')
    }
    return(ret)
  }
  
  #XXX check spec validity here.  e.g. column three should be convertible to integer
  
  i = 1
  
  while ( i <= length(opt) ) {
    if ( debug ) print(paste("processing",opt[i]))
    
    current.flag = 0 #XXX use NA
    optstring = opt[i]
    
    
    #long flag
    if ( substr(optstring, 1, 2) == '--' ) {
      if ( debug ) print(paste("  long option:",opt[i]))
      
      optstring = substring(optstring,3)
      
      this.flag = NA
      this.argument = NA
      kv = strsplit(optstring, '=')[[1]]
      # if ( !is.na(kv[2]) ) {
      if ( grepl('=', optstring) ) {
        this.flag = kv[1]
        this.argument = paste(kv[-1], collapse="=")
      } else {
        this.flag = optstring
      }
      
      rowmatch = grep( this.flag, spec[,col.long.name],fixed=TRUE )
      
      #long flag is invalid, matches no options
      if ( length(rowmatch) == 0 ) {
        stop(paste('long flag "', this.flag, '" is invalid', sep=''))
        
        #long flag is ambiguous, matches too many options
      } else if ( length(rowmatch) > 1 ) {
        # check if there is an exact match and use that
        rowmatch = which(this.flag == spec[,col.long.name])
        if(length(rowmatch) == 0) {
          stop(paste('long flag "', this.flag, '" is ambiguous', sep=''))
        }
      }
      
      #if we have an argument
      if ( !is.na(this.argument) ) {
        #if we can't accept the argument, bail out
        if ( spec[rowmatch, col.has.argument] == flag.no.argument ) {
          stop(paste('long flag "', this.flag, '" accepts no arguments', sep=''))
          
          #otherwise assign the argument to the flag
        } else {
          mode = spec[rowmatch, col.mode]
          warning_msg <- tryCatch(storage.mode(this.argument) <- mode,
                                  warning = function(w) {warning(paste(mode, "expected, got", dQuote(this.argument)))})
          if( is.na(this.argument) && !grepl("expected, got",  warning_msg) ) {
            warning(paste('long flag', this.flag, 'given a bad argument'))
          }
          result[spec[rowmatch, col.long.name]] = this.argument
          i = i + 1
          next
        }
        
        #otherwise, we don't have an argument
      } else {
        #if we require an argument, bail out
        ###if ( spec[rowmatch, col.has.argument] == flag.required.argument ) {
        ###  stop(paste('long flag "', this.flag, '" requires an argument', sep=''))
        
        #long flag has no attached argument. set flag as present.  set current.flag so we can peek ahead later and consume the argument if it's there
        ###} else {
        result[spec[rowmatch, col.long.name]] = TRUE
        current.flag = rowmatch
        ###}
      }
      
      #short flag(s)
    } else if ( substr(optstring, 1, 1) == '-' ) {
      if ( debug ) print(paste("  short option:",opt[i]))
      
      these.flags = strsplit(optstring,'')[[1]]
      
      done = FALSE
      for ( j in 2:length(these.flags) ) {
        this.flag = these.flags[j]
        rowmatch = grep( this.flag, spec[,col.short.name],fixed=TRUE )
        
        #short flag is invalid, matches no options
        if ( length(rowmatch) == 0 ) {
          stop(paste('short flag "', this.flag, '" is invalid', sep=''))
          
          #short flag has an argument, but is not the last in a compound flag string
        } else if ( j < length(these.flags) & spec[rowmatch,col.has.argument] == flag.required.argument ) {
          stop(paste('short flag "', this.flag, '" requires an argument, but has none', sep=''))
          
          #short flag has no argument, flag it as present
        } else if ( spec[rowmatch,col.has.argument] == flag.no.argument ) {
          result[spec[rowmatch, col.long.name]] = TRUE
          done = TRUE
          
          #can't definitively process this flag yet, need to see if next option is an argument or not
        } else {
          result[spec[rowmatch, col.long.name]] = TRUE
          current.flag = rowmatch
          done = FALSE
        }
      }
      if ( done ) {
        i = i + 1
        next
      }
    }
    
    #invalid opt
    if ( current.flag == 0 ) {
      stop(paste('"', optstring, '" is not a valid option, or does not support an argument', sep=''))
      #TBD support for positional args
      #if ( debug ) print(paste('"', optstring, '" not a valid option.  It is appended to getopt(...)$ARGS', sep=''))
      #result$ARGS = append(result$ARGS, optstring)
      
      # some dangling flag, handle it
    } else if ( current.flag > 0 ) {
      if ( debug ) print('    dangling flag')
      if ( length(opt) > i ) {
        peek.optstring = opt[i + 1]
        if ( debug ) print(paste('      peeking ahead at: "',peek.optstring,'"',sep=''))
        
        #got an argument.  attach it, increment the index, and move on to the next option.  we don't allow arguments beginning with '-' UNLESS
        #specfile indicates the value is an "integer" or "double", in which case we allow a leading dash (and verify trailing digits/decimals).
        if ( substr(peek.optstring, 1, 1) != '-' |
             #match negative double
             ( substr(peek.optstring, 1, 1) == '-'
               & regexpr("^-[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", peek.optstring) > 0
               & spec[current.flag, col.mode]== 'double'
             ) |
             #match negative integer
             ( substr(peek.optstring, 1, 1) == '-'
               & regexpr("^-[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", peek.optstring) > 0
               & spec[current.flag, col.mode]== 'integer'
             )
        ) {
          if ( debug ) print(paste('        consuming argument *',peek.optstring,'*',sep=''))
          
          # storage.mode(peek.optstring) = spec[current.flag, col.mode]
          mode = spec[current.flag, col.mode]
          tryCatch(storage.mode(peek.optstring) <- mode,
                   warning = function(w) {warning(paste(mode, "expected, got", dQuote(peek.optstring)))})
          result[spec[current.flag, col.long.name]] = peek.optstring
          i = i + 1
          
          #a lone dash
        } else if ( substr(peek.optstring, 1, 1) == '-' & length(strsplit(peek.optstring,'')[[1]]) == 1 ) {
          if ( debug ) print('        consuming "lone dash" argument')
          # storage.mode(peek.optstring) = spec[current.flag, col.mode]
          mode = spec[current.flag, col.mode]
          tryCatch(storage.mode(peek.optstring) <- mode,
                   warning = function(w) {warning(paste(mode, "expected, got", dQuote(peek.optstring)))}) #nocov 
          result[spec[current.flag, col.long.name]] = peek.optstring
          i = i + 1
          
          #no argument
        } else {
          if ( debug ) print('        no argument!')
          
          #if we require an argument, bail out
          if ( spec[current.flag, col.has.argument] == flag.required.argument ) {
            stop(paste('flag "', this.flag, '" requires an argument', sep=''))
            
            #otherwise set flag as present.
          } else if (
            spec[current.flag, col.has.argument] == flag.optional.argument |
            spec[current.flag, col.has.argument] == flag.no.argument 
          ) {
            x = TRUE
            storage.mode(x) = spec[current.flag, col.mode]
            result[spec[current.flag, col.long.name]] = x
          } else {
            stop(paste("This should never happen.", #nocov
                       "Is your spec argument correct?  Maybe you forgot to set", #nocov
                       "ncol=4, byrow=TRUE in your matrix call?")) #nocov
          }
        }
        #trailing flag without required argument
      } else if ( spec[current.flag, col.has.argument] == flag.required.argument ) {
        stop(paste('flag "', this.flag, '" requires an argument', sep=''))
        
        #trailing flag without optional argument
      } else if ( spec[current.flag, col.has.argument] == flag.optional.argument ) {
        x = TRUE
        storage.mode(x) = spec[current.flag, col.mode]
        result[spec[current.flag, col.long.name]] = x
        
        #trailing flag without argument
      } else if ( spec[current.flag, col.has.argument] == flag.no.argument ) {
        x = TRUE
        storage.mode(x) = spec[current.flag, col.mode]
        result[spec[current.flag, col.long.name]] = x
      } else {
        stop("this should never happen (2).  please inform the author.") #nocov
      }
    } #no dangling flag, nothing to do.
    
    i = i+1
  }
  return(result)
}
get_Rscript_filename = function() {
  prog <- sub("--file=", "", grep("--file=", commandArgs(), value=TRUE)[1])
  if( .Platform$OS.type == "windows") { 
    prog <- gsub("\\\\", "\\\\\\\\", prog)
  }
  prog
}
print_header = function(){
  version="V1.0"
  cat("###################################################\n")
  cat("## Sustitution Segments Detection (SSD)\n")
  cat(paste0("## Version: ", version,"\n"))
  cat("## Written by: Guo-An Qi, Zhejiang University\n")
  cat("## Bug report: guoan.qi@foxmail.com\n")
  cat("###################################################\n\n")
}
sourceCpp(code = '
#include <Rcpp.h>
#include <vector>
#include <iterator>
#include <algorithm>
using namespace Rcpp;
       
//[[Rcpp::export]]
IntegerMatrix doSlidingWindowCompute(IntegerVector varClass, IntegerVector windowL, IntegerVector windowR, IntegerVector varCrd){
  
  // Initialize
  IntegerMatrix resultMat(windowL.size(), 4);
  IntegerVector noIndex(windowL.size());         // 0
  IntegerVector ambiguousIndex(windowL.size());  // 1
  IntegerVector yesIndex(windowL.size());        // 2
  IntegerVector sumIndex(windowL.size());

  for(int i=0; i<windowL.size(); i++){
    int idxL=0;
    int idxR=0;
    // determine the index left and right range 
    for(int j=0; j<varCrd.size(); j++) { if(varCrd[j] >= windowL[i]) {idxL=j; break;}}
    for(int j=0; j<varCrd.size(); j++) { if(varCrd[j] >= windowR[i]) {idxR=j; break;}}
    
    // in simulation there windowR may be larger than the biggest varCrd
    if(idxR==0) {idxR=varCrd.size()-1;}
    
    // count 0,1,2
    for(int j=idxL; j<=idxR; j++){
      if(varClass[j]==0){
        noIndex[i]=noIndex[i]+1;
      }else if(varClass[j]==1){
        ambiguousIndex[i]=ambiguousIndex[i]+1;
      }else{
        yesIndex[i]=yesIndex[i]+1;
      }
    }
    
    // sum
    sumIndex[i]=noIndex[i]+ambiguousIndex[i]+yesIndex[i];
  }
  
  resultMat(_,0)=sumIndex;
  resultMat(_,1)=yesIndex;
  resultMat(_,2)=ambiguousIndex;
  resultMat(_,3)=noIndex;
  
  return resultMat;
}
')

command = matrix(c("tfile","t",1,"character", "plink variant-major additive component file",
                   "ap","a",1,"character", "ID of adaptive parental line",
                   "dp","d",1,"character","ID of donor parental line",
                   "adj","c",2,"numeric","baselevel of adjustment for inferring the parental origin of ambiguous alleles (NLSA), genomic region with LSP lower than 0.8 but higher than the given value would shift to adjustment process, optional, defaulted 0.7",
                   "err","e",2,"numeric","error rate tolerance of the adjustment process, optional, defaulted 0.05",
                   "window","w",2,"numeric","window size(bp)of sliding window analysis for calculate the initial LSP, optional, defaulted 1000000",
                   "step","s",2,"numeric","step size (bp) of sliding window analysis for calculate the initial LSP, optional, defaulted 20000",
                   "out","o",2,"character","specify prefix of output results, optional, defaulted SSD",
                   "help","h",0,"logical", "parameters input instruction"),
                 byrow=T,ncol=5)
args = getopt(spec = command)

if (!is.null(args$help) || is.null(args$tfile) || is.null(args$ap) || is.null(args$dp)) {
  print_header()
  cat(paste(getopt(command, usage = T), "\n"))
  q()
}

# grab arguements
tfile = args$tfile
ap = args$ap
dp = args$dp

adj = 0.7
err = 0.05
window = 1000000
step = 20000
out = "SSD"
fraction = 0.8

if (!is.null(args$adj)) {
  adj = args$adj
}
if (!is.null(args$err)) {
  err = args$err
}
if (!is.null(args$window)) {
  window = args$window
}
if (!is.null(args$step)) {
  step = args$step
}
if (!is.null(args$out)) {
  out = args$out
}
print_header()
cat(paste0("Options: \n  --tfile ",tfile," \n  --ap ",ap," \n  --dp ",dp," \n  --adj ",adj," \n  --err ", err," \n  --window ",window," \n  --step ", step," \n  --out ",out,"\n\n"))

setwd(".")
# pre-read file
dt=fread(tfile,header=T,nrows = 10)

IL.list=colnames(dt)[-c(1:6)]
PL.index=which(IL.list==ap | IL.list==dp)
IL.list=IL.list[-PL.index]

for(i in 1:length(IL.list)){
  time1=proc.time()
  currentSample=IL.list[i]
  cat(paste0("Detecting introgression regions for ",currentSample," ...\n"))
  targetColumn1=which(colnames(dt)==ap)
  targetColumn2=which(colnames(dt)==dp)
  targetColumn3=which(colnames(dt)==currentSample)
  targetColumn=c(targetColumn1,targetColumn2,targetColumn3)
  
  classes=sapply(dt, class)
  classes[-targetColumn]=rep("NULL",length(classes)-length(targetColumn))
  classes[c(1,4)]='character'
  data=fread(tfile,header=T,colClasses = classes)

  updateColumn_target=which(colnames(data)==currentSample)
  updateColumn_ap=which(colnames(data)==ap)
  updateColumn_dp=which(colnames(data)==dp)
  
  # remove variants without polymorphisms and variants with missing genotype in CSSL
  RA_index=Reduce(union,list(which(data[,..updateColumn_ap]==0 & data[,..updateColumn_dp]==0),
                             which(data[,..updateColumn_ap]==2 & data[,..updateColumn_dp]==2),
                             which(is.na(data[,..updateColumn_target]) | is.na(data[,..updateColumn_ap]) | is.na(data[,..updateColumn_dp]))))
  if(length(RA_index)>0){
    data=data[-RA_index,]
  }
  data$introgression=NA
  introgress=Reduce(union,list(which(data[,..updateColumn_ap]==0 & data[,..updateColumn_dp]>0 & data[,..updateColumn_target]>0),
    which(data[,..updateColumn_ap]==2 & data[,..updateColumn_dp]<2 & data[,..updateColumn_target]<2)
    ))

  non_introgress=Reduce(union,list(which(data[,..updateColumn_ap]==0 & data[,..updateColumn_dp]==2 & data[,..updateColumn_target]==0),
    which(data[,..updateColumn_ap]==1 & data[,..updateColumn_dp]==0 & data[,..updateColumn_target]==2),
    which(data[,..updateColumn_ap]==1 & data[,..updateColumn_dp]==2 & data[,..updateColumn_target]==0),
    which(data[,..updateColumn_ap]==2 & data[,..updateColumn_dp]==0 & data[,..updateColumn_target]==2)
    ))

  ambiguous=setdiff(1:nrow(data),union(introgress,non_introgress))

  data$introgression[introgress]=2 #Yes
  data$introgression[non_introgress]=0 #No
  data$introgression[ambiguous]=1 #Ambiguous

  # write.table(paste(data$CHR[which(data$introgression==2)],data$POS[which(data$introgression==2)],sep = ":"),
  #             paste0(out,".",currentSample,"_LSA.txt"),
  #             quote=F,col.names = F,row.names = F)
  chrList=unique(data$CHR)
  Intro.dt=data.frame()
  Intro.summary=data.frame()
  for(j in 1:length(chrList)){
    currentCHR=chrList[j]
    CHR_dt=data[which(data$CHR==currentCHR),]
    CHR_dt$POS=as.numeric(CHR_dt$POS)
    CHR_dt=CHR_dt[order(CHR_dt$POS,decreasing = F),]
    MAX=max(CHR_dt$POS)

    start=1
    windowL = seq(from=start, to=(MAX-window), by=step)
    windowL = c(windowL,MAX-window)
    windowR = windowL+window
    slide_dt=doSlidingWindowCompute(CHR_dt$introgression,windowL,windowR,CHR_dt$POS)

    tempDT=as.data.frame(cbind(CHR=currentCHR,START=windowL,END=windowR,slide_dt),stringsAsFactors = F)
    colnames(tempDT)=c("CHR","START","END","NUMBER","PROPORTION_INTRO","PROPORTION_AMBIGUOUS","PROPORTION_NONINTRO")
    tempDT$START=as.numeric(tempDT$START)
    tempDT$END=as.numeric(tempDT$END)
    tempDT$NUMBER=as.numeric(tempDT$NUMBER)
    tempDT$PROPORTION_INTRO=as.numeric(tempDT$PROPORTION_INTRO)/tempDT$NUMBER
    tempDT$PROPORTION_NONINTRO=as.numeric(tempDT$PROPORTION_NONINTRO)/tempDT$NUMBER
    tempDT$PROPORTION_AMBIGUOUS=as.numeric(tempDT$PROPORTION_AMBIGUOUS)/tempDT$NUMBER

    tempDT$PROPORTION_INTRO_UPDATE=ifelse(tempDT$PROPORTION_NONINTRO<err & tempDT$PROPORTION_INTRO>adj,tempDT$PROPORTION_INTRO+tempDT$PROPORTION_AMBIGUOUS,tempDT$PROPORTION_INTRO)
    Intro.dt=rbind(Intro.dt,tempDT)

    tempDT=tempDT[which(tempDT$NUMBER>1),]
    tempDT=tempDT[which(as.numeric(tempDT[,"PROPORTION_INTRO_UPDATE"])>=fraction),1:3]
    if(nrow(tempDT)==0){
      next
    } else {
      collapse_introgressRegion = data.frame(
        tempDT %>%
          arrange(START) %>%
          group_by(g = cumsum(cummax(lag(END, default = first(END))) < START)) %>%
          summarise(START = first(START), END = max(END)))

      for(k in 1:nrow(collapse_introgressRegion)){
        left_border_old=collapse_introgressRegion[k,2]
        right_border_old=collapse_introgressRegion[k,3]

        interval_variants=which(CHR_dt$POS>=left_border_old & CHR_dt$POS<=right_border_old)
        collapse_introgressRegion[k,4]=length(interval_variants)

        interval_variants=intersect(interval_variants,which(CHR_dt$introgression==2))
        left_border_update=min(CHR_dt$POS[interval_variants])
        right_border_update=max(CHR_dt$POS[interval_variants])

        collapse_introgressRegion[k,2]=left_border_update
        collapse_introgressRegion[k,3]=right_border_update
      }
      collapse_introgressRegion$g=currentCHR
      colnames(collapse_introgressRegion)[c(1,4)]=c('CHR','NUM')
      Intro.summary=rbind(Intro.summary,collapse_introgressRegion)
    }
  }
  colnames(Intro.dt)[c(4:8)]=c("VARIANT_NUMBER","INITIAL_LSP/DONOR_LSA","NLSA","ADAPTIVE_LSA","UPDATED_LSP/DONOR_LSA")
  outFile1=paste0(out,".",currentSample,".blockwise.intro")
  write.table(Intro.dt,outFile1,quote=F,col.names = T,row.names = F,sep='\t')
  if(nrow(Intro.summary)>0){
    colnames(Intro.summary)[4]="VARIANT_NUMBER"
    outFile2=paste0(out,".",currentSample,".blockwise.collapse.intro")
    write.table(Intro.summary,outFile2,quote=F,col.names = T,row.names = F,sep='\t')
  }

  scaffold_index=c(grep("Scaffold",Intro.dt$CHR),grep("scaffold",Intro.dt$CHR))
  if(length(scaffold_index)>0){
    plot_intro=Intro.dt[-scaffold_index,c(1,2,8)]
  } else {
    plot_intro=Intro.dt[,c(1,2,8)]
  }

  plot_intro$START=plot_intro$START / 1000000
  colnames(plot_intro)[3]='LSP'
  plot_intro$TYPE=ifelse(plot_intro$LSP>=0.8,'Introgress-segment','Native-segment')
  plot_break=seq(0,ceiling(max(plot_intro$START)),10)

  intro_plot=ggplot(data = plot_intro) +
    geom_point(aes(x=START, y=LSP, colour = TYPE), size = 0.6) +
    scale_colour_manual(values=c("Introgress-segment" = "#f4a069", "Native-segment" = "#9dc3e6"))+
    scale_x_continuous(breaks=plot_break) +
    facet_wrap(~ CHR, ncol = 2, strip.position="left") +
    labs(y="Local substitution proportion (LSP)", x="Chromosomal position (Mbp)") +
    theme_bw(base_size = 13) +
    theme(strip.background =element_rect(fill="lightgrey"), legend.position = "None") +
    coord_cartesian(ylim = c(0,1))
  ggsave(paste0(out,'.',currentSample,'.png'), intro_plot, width=16, height=16, device='png')
  
  time2=proc.time()
  time = (time2-time1)[3][[1]]
  cat(paste0("  Finished! Elapsed time: ", time/60," minutes.\n\n"))
}