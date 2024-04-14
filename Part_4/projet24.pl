% Zhiyu Lin 300255509, Yitao Cui 300345709

% dataset(DirectoryName)
% this is where the image dataset is located

dataset('D:/My_Things/University/2024_Winter/CSI2120/CSI2120_Project/Part_4/imageDataset2_15_20/').     % set dataset image folder path
queryset('D:/My_Things/University/2024_Winter/CSI2120/CSI2120_Project/Part_4/queryImages/').            % set query image folder path

% directory_textfiles(DirectoryName, ListOfTextfiles)
% produces the list of text files in a directory
directory_textfiles(D,Textfiles):- directory_files(D,Files), include(isTextFile, Files, Textfiles).
isTextFile(Filename):-string_concat(_,'.txt',Filename).
% read_hist_file(Filename,ListOfNumbers)
% reads a histogram file and produces a list of numbers (bin values)
read_hist_file(Filename,Numbers):- open(Filename,read,Stream),read_line_to_string(Stream,_),
                                   read_line_to_string(Stream,String), close(Stream),
								   atomic_list_concat(List, ' ', String),atoms_numbers(List,Numbers).
								   

% similarity_search(QueryFile,SimilarImageList)
% returns the list of images similar to the query image
% similar images are specified as (ImageName, SimilarityScore)
% predicat dataset/1 provides the location of the image set

similarity_search(QueryFile, SimilarList) :-
    dataset(D), directory_textfiles(D, TxtFiles),
    queryset(Q),
    string_concat(Q, QueryFile, QueryPath),
    similarity_search(QueryPath, D, TxtFiles, SimilarList).

									
% similarity_search(QueryFile, DatasetDirectory, HistoFileList, SimilarImageList)
similarity_search(QueryFile,DatasetDirectory, DatasetFiles,Best):- read_hist_file(QueryFile,QueryHisto), 
                                            compare_histograms(QueryHisto, DatasetDirectory, DatasetFiles, Scores), 
                                            sort(2,@>,Scores,Sorted),take(Sorted,5,Best).


% compare_histograms(QueryHisto,DatasetDirectory,DatasetFiles,Scores)
% compares a query histogram with a list of histogram files 

compare_histograms(_, _, [], []).
compare_histograms(QueryHisto, DatasetDirectory, [H|T], [(H, Score)|Scores]) :-
    string_concat(DatasetDirectory, H, FilePath),
    read_hist_file(FilePath, DatasetHisto),

    
    % compute histogram similarity (Normalized)
    normalize_histogram(QueryHisto, NormalizedQueryHisto),
    normalize_histogram(DatasetHisto, NormalizedDatasetHisto),
    histogram_intersection(NormalizedQueryHisto, NormalizedDatasetHisto, Score),    

    % histogram_intersection(QueryHisto, DatasetHisto, Score),          % compute histogram similarity (Not normalized)
    
    compare_histograms(QueryHisto, DatasetDirectory, T, Scores).


% Normalizes the histogram, remove this if not normalize the histograms.

normalize_elements([], _, []).
normalize_elements([H|T], Total, [N|NormalizedTail]) :-
    (Total > 0 -> N is H / Total; N = 0),  % avoid divide 0
    normalize_elements(T, Total, NormalizedTail).

normalize_histogram(Histogram, NormalizedHistogram) :-
    sum_list(Histogram, Total),
    normalize_elements(Histogram, Total, NormalizedHistogram).


% calculate the sum of the minimum values between two histograms
sum_min([], [], 0).
sum_min([A|As], [B|Bs], Total) :-
    sum_min(As, Bs, Rest),
    Min is min(A, B),
    Total is Rest + Min.

% histogram_intersection(Histogram1, Histogram2, Score)
% compute the intersection similarity score between two histograms
% Score is between 0.0 and 1.0 (1.0 for identical histograms)

histogram_intersection(H1, H2, S) :-
    sum_min(H1, H2, MinSum),
    sum_list(H1, SumH1),
    RawScore is MinSum / SumH1,
    (RawScore >= 0.9999999999999998 -> S = 1.0 ; S = RawScore). % ensure that similarity is always [0,1]




% take(List,K,KList)
% extracts the K first items in a list
take(Src,N,L) :- findall(E, (nth1(I,Src,E), I =< N), L).
% atoms_numbers(ListOfAtoms,ListOfNumbers)
% converts a list of atoms into a list of numbers
atoms_numbers([],[]).
atoms_numbers([X|L],[Y|T]):- atom_number(X,Y), atoms_numbers(L,T).
atoms_numbers([X|L],T):- \+atom_number(X,_), atoms_numbers(L,T).
