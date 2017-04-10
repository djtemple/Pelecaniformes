order(pelecaniformes).

family(pelecanidae).
family(ardeidae).
family(threskiornithidae).

genus(pelecanus).
genus(botaurus).
genus(ixobrychus).
genus(ardea).
genus(egretta).
genus(bubulcus).
genus(butorides).
genus(nycticorax).
genus(nyctanassa).
genus(eudocimus).
genus(plegadis).
genus(platalea).

species(erythrorhynchos).
species(occidentalis).
species(lentiginosus).
species(exilis).
species(herodias).
species(alba).
species(thula).
species(caerulea).
species(tricolor).
species(rufescens).
species(ibis).
species(virescens).
species(nycticorax).
species(violacea).
species(albus).
species(falcinellus).
species(chihi).
species(ajaja).

hasParent(pelecanidae, pelecaniformes).
hasParent(ardeidae, pelecaniformes).
hasParent(threskiornithidae, pelecaniformes).
hasParent(pelecanus, pelecanidae).
hasParent(botaurus, ardeidae).
hasParent(ixobrychus, ardeidae).
hasParent(ardea, ardeidae).
hasParent(egretta, ardeidae).
hasParent(bubulcus, ardeidae).
hasParent(butorides, ardeidae).
hasParent(nycticorax, ardeidae).
hasParent(nyctanassa, ardeidae).
hasParent(eudocimus, threskiornithidae).
hasParent(plegadis, threskiornithidae).
hasParent(platalea, threskiornithidae).
hasParent(erythrorhynchos, pelecanus).
hasParent(occidentalis, pelecanus).
hasParent(lentiginosus, botaurus).
hasParent(exilis, ixobrychus).
hasParent(herodias, ardea).
hasParent(alba, ardea).
hasParent(thula, egretta).
hasParent(caerulea, egretta).
hasParent(tricolor, egretta).
hasParent(rufescens, egretta).
hasParent(ibis, bubulcus).
hasParent(virescens, butorides).
hasParent(nycticorax, nycticorax).
hasParent(violacea, nyctanassa).
hasParent(albus, eudocimus).
hasParent(falcinellus, plegadis).
hasParent(chihi, plegadis).
hasParent(ajaja, platalea).

hasParent2(nycticorax, ardeidae).
hasParent2(X,Y) :- hasParent(X,Y) *-> \+ species(X).
hasParent2(X,Y) :- hasCompoundName(Y,_,X).

hasCompoundName(G,S,C) :- genus(G), species(S), hasParent(S,G), atom_concat(G, '_', X), atom_concat(X,S,C).

hasCommonName(pelecanus, pelican).
hasCommonName(pelecanus_erythrorhynchos, americanWhitePelican).
hasCommonName(pelecanus_occidentalis, brownPelican).
hasCommonName(botaurus, bittern).
hasCommonName(botaurus_lentiginosus, americanBittern).
hasCommonName(ixobrychus, bittern).
hasCommonName(ixobrychus_exilis, leastBittern).
hasCommonName(ardea, heron).
hasCommonName(ardea_herodias, greatBlueHeron).
hasCommonName(ardea_alba, greatEgret).
hasCommonName(egretta, heron).
hasCommonName(egretta, egret).
hasCommonName(egretta_thula, snowyEgret).
hasCommonName(egretta_caerulea, littleBlueHeron).
hasCommonName(egretta_tricolor, tricoloredHeron).
hasCommonName(egretta_rufescens, reddishEgret).
hasCommonName(bubulcus, egret).
hasCommonName(bubulcus_ibis, cattleEgret).
hasCommonName(butorides, heron).
hasCommonName(butorides_virescens, greenHeron).
hasCommonName(nycticorax, nightHeron).
hasCommonName(nycticorax_nycticorax, blackCrownedNightHeron).
hasCommonName(nyctanassa, nightHeron).
hasCommonName(nyctanassa_violacea, yellowCrownedNightHeron).
hasCommonName(eudocimus,ibis).
hasCommonName(eudocimus_albus, whiteIbis).
hasCommonName(plegadis, ibis).
hasCommonName(plegadis_falcinellus, glossyIbis).
hasCommonName(plegadis_chihi, whiteFacedIbis).
hasCommonName(platalea, spoonbill).
hasCommonName(platalea_ajaja, roseateSpoonbill).

hasCommonName(G,S,C) :- hasCompoundName(G,S,Compound), hasCommonName(Compound,C).

hasSciName(C,N) :- hasCommonName(N,C).

isaStrict(A,A).
isaStrict(A,B) :- hasParent2(A,C), isaStrict(C,B).

isa(A,B) :-	(nonvar(A), nonvar(B)) -> hasCommonName(X,A), hasCommonName(Y,B),isaStrict(X,Y).
isa(A,B) :- nonvar(A) -> hasCommonName(X,A), isaStrict(X,B).
isa(A,B) :- nonvar(B) -> hasCommonName(Y,B), isaStrict(A,Y).
isa(A,B) :- isaStrict(A,B).

synonym(A,B) :- hasCommonName(A,B).
synonym(A,B) :- hasSciName(A,B).
synonym(A,B) :- hasSciName(A,X), hasSciName(B,X), A \= B, !.

habitat(X,Y) :- atom(X) ->  isaStrict(S,X), habitat1(S,Y).
habitat(X,Y) :- var(X) -> habitat1(X,Y).

habitat1(pelecanus_erythrorhynchos, lakePond).
habitat1(pelecanus_occidentalis, ocean).
habitat1(botaurus_lentiginosus, marsh).
habitat1(ixobrychus_exilis, marsh).
habitat1(ardea_herodias, marsh).
habitat1(ardea_alba, marsh).
habitat1(egretta_thula, marsh).
habitat1(egretta_caerulea, marsh).
habitat1(egretta_tricolor, marsh).
habitat1(egretta_rufescens, marsh).
habitat1(bubulcus_ibis, marsh).
habitat1(butorides_virescens, marsh).
habitat1(nycticorax_nycticorax, marsh).
habitat1(nyctanassa_violacea,  marsh).
habitat1(eudocimus_albus, marsh).
habitat1(plegadis_falcinellus, marsh).
habitat1(plegadis_chihi, marsh).
habitat1(platalea_ajaja, marsh).

nesting(X,Y) :- atom(X) -> isaStrict(S,X), nesting1(S,Y).
nesting(X,Y) :- nesting1(X,Y).

nesting1(pelecanus_erythrorhynchos, ground).
nesting1(pelecanus_occidentalis, tree).
nesting1(botaurus_lentiginosus, ground).
nesting1(ixobrychus_exilis, ground).
nesting1(ardea_herodias, tree).
nesting1(ardea_alba, tree).
nesting1(egretta_thula, tree).
nesting1(egretta_caerulea, tree).
nesting1(egretta_tricolor, tree).
nesting1(egretta_rufescens, tree).
nesting1(bubulcus_ibis, tree).
nesting1(butorides_virescens, tree).
nesting1(nycticorax_nycticorax, tree).
nesting1(nyctanassa_violacea, tree).
nesting1(eudocimus_albus, tree).
nesting1(plegadis_falcinellus, ground).
nesting1(plegadis_chihi, ground).
nesting1(platalea_ajaja, tree).

food(X,Y) :- var(X) -> hasCompoundName(_,_,X), food1(X,Y).
food(X,Y) :- atom(X) -> isaStrict(S,X), hasCompoundName(_,_,S), food1(S,Y).

food1(pelecanus_erythrorhynchos, fish).
food1(pelecanus_occidentalis, fish).
food1(botaurus_lentiginosus, fish).
food1(ixobrychus_exilis, fish).
food1(ardea_herodias, fish).
food1(ardea_alba, fish).
food1(egretta_thula, fish).
food1(egretta_caerulea, fish).
food1(egretta_tricolor, fish).
food1(egretta_rufescens, fish).
food1(bubulcus_ibis, insects).
food1(butorides_virescens, fish).
food1(nycticorax_nycticorax, fish).
food1(nyctanassa_violacea,  insects).
food1(eudocimus_albus, insects).
food1(plegadis_falcinellus, insects).
food1(plegadis_chihi, insects).
food1(platalea_ajaja, fish).

conservation(egretta_rufescens, nt).
conservation(X, lc) :- var(X), hasCompoundName(_,_,X), X \= egretta_rufescens.

%returns the behaviour associated with a given bird
behavior(X,Y):- var(X) -> hasCompoundName(_,_,X), behaves(X,Y).
behavior(X,Y):- atom(X) -> isaStrict(S,X), behaves(S,Y).

%A list of all the bird behaviours

behaves(pelecanus_erythrorhynchos, surfaceDive).
behaves(pelecanus_occidentalis, aerialDive).
behaves(botaurus_lentiginosus, stalking).
behaves(ixobrychus_exilis, stalking).
behaves(ardea_herodias, stalking).
behaves(ardea_alba, stalking).
behaves(egretta_thula, stalking).
behaves(egretta_caerulea, stalking).
behaves(egretta_tricolor, stalking).
behaves(egretta_rufescens, stalking).
behaves(bubulcus_ibis, groundForager).
behaves(butorides_virescens, stalking).
behaves(nycticorax_nycticorax, stalking).
behaves(nyctanassa_violacea, stalking).
behaves(eudocimus_albus, probing).
behaves(plegadis_falcinellus, probing).
behaves(plegadis_chihi, probing).
behaves(platalea_ajaja, probing).


%returns the range of a given bird
rangesTo(X,Y):- var(X) -> hasCompoundName(_,_,X), ranges(X,Y).
rangesTo(X,Y) :- atom(X) -> isaStrict(S,X), ranges(S,Y).

% a list of all the ranges of the birds
ranges(pelecanus_erythrorhynchos, alberta).
ranges(botaurus_lentiginosus, alberta).
ranges(bubulcus_ibis, canada).
ranges(ardea_herodias, alberta).
ranges(ardea_alba, canada).
ranges(butorides_virescens, canada).
ranges(nycticorax_nycticorax, alberta).
ranges(X, canada) :- ranges(X, alberta).
