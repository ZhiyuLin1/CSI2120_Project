% dataset(DirectoryName)
% this is where the image dataset is located

dataset('D:/My_Things/University/2024_Winter/CSI2120/CSI2120_Project/Part_4/imageDataset2_15_20/').
qset('D:/My_Things/University/2024_Winter/CSI2120/CSI2120_Project/Part_4/queryImages/').

% directory_textfiles(DirectoryName, ListOfTextfiles)
% produces the list of text files in a directory


directory_textfiles(D,Textfiles):- 
directory_files(D,Files), 
include(isTextFile, Files, Textfiles).

isTextFile(Filename):-
string_concat(_,'.txt',Filename).


% read_hist_file(Filename,ListOfNumbers)
% reads a histogram file and produces a list of numbers (bin values)

read_hist_file(Filename,Numbers):- 
open(Filename,read,Stream),
read_line_to_string(Stream,_),
read_line_to_string(Stream,String), 
close(Stream),
atomic_list_concat(List, ' ', String),
atoms_numbers(List,Numbers).
								   
% similarity_search(QueryFile,SimilarImageList)
% returns the list of images similar to the query image
% similar images are specified as (ImageName, SimilarityScore)
% predicat dataset/1 provides the location of the image set


similarity_search(QueryFile,SimilarList) :- 
dataset(D), 
directory_textfiles(D,TxtFiles),
similarity_search(QueryFile,D,TxtFiles,SimilarList).
											
% similarity_search(QueryFile, DatasetDirectory, HistoFileList, SimilarImageList)

similarity_search(QueryFile,DatasetDirectory, DatasetFiles,Best):- 
qset(Q), 
string_concat(Q,QueryFile,QueryFilePath),
write(QueryFilePath),
read_hist_file(QueryFilePath,QueryHisto), 
compare_histograms(QueryHisto, DatasetDirectory, DatasetFiles, Scores), 
sort(2,@>,Scores,Sorted),
take(Sorted,5,Best).

% compare_histograms(QueryHisto,DatasetDirectory,DatasetFiles,Scores)
% compares a query histogram with a list of histogram files 

% Base case: if DatasetFiles is empty returns [] 

compare_histograms(_,_,[],[]).

% if DatasetFiles is not empty, calculate the first file Histogram and add to Scores recusively 

compare_histograms(H1,DatasetDirectory,[Head|T],Scores):-
string_concat(DatasetDirectory,Head,Filepath),
read_hist_file(Filepath,H2),
sum(H1,S1),
sum(H2,S2),
normalize(H1,S1,NH1),
normalize(H2,S2,NH2),
histogram_intersection(NH1,NH2,Intersection),
compare_histograms(H1,DatasetDirectory,T,PScores),
Score=(Head,Intersection),
Scores=[Score|PScores].





% histogram_intersection(Histogram1, Histogram2, Score)
% compute the intersection similarity score between two histograms
% Score is between 0.0 and 1.0 (1.0 for identical histograms)


histogram_intersection(_,[],0):-!.
histogram_intersection([],_,0):-!.

histogram_intersection([X|H1T],[Y|H2T],S):-
histogram_intersection(H1T,H2T,PS),
min(X,Y,R),
S is PS + R.


%find min

min(X,Y,R):-
X>=Y,
R=Y.

min(X,Y,R):-
Y>X,
R=X.




%find sum of the pixels

sum([],0).
sum([H|T],S):-
sum(T,Ps),
S is Ps + H.


%normalize the histogram

normalize([],_,[]).

normalize([X|T],S,Nh):-
normalize(T,S,PNh),
Nx is X/S,
Nh=[Nx|PNh].





% take(List,K,KList)
% extracts the K first items in a list

take(Src,N,L) :- 
findall(E, (nth1(I,Src,E), I =< N), L).

% atoms_numbers(ListOfAtoms,ListOfNumbers)
% converts a list of atoms into a list of numbers

atoms_numbers([],[]).
atoms_numbers([X|L],[Y|T]):- atom_number(X,Y), atoms_numbers(L,T).
atoms_numbers([X|L],T):- \+atom_number(X,_), atoms_numbers(L,T).