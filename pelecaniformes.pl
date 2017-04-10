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
hasCommonName(nyctanassa_violacea, yelloCrownedNightHeron).
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

habitat(pelecanus_erythrorynchos, marsh).
habitat(pelecanus_occidentalus, ocean).
habitat(botaurus_lentiginosus, marsh).
habitat(ardea_herodias, marsh).
habitat(ardea_herodias, lakePond).
habitat(ardea_herodias, ocean).
habitat(ardea_alba, ocean).
habitat(egretta_thula, ocean).
habitat(egretta_thula, marsh).
habitat(X,Y) :- nonvar(X) -> isaStrict(S,X), habitat(S,Y).

nesting(pelecanus_erythrorhynchos, ground).
nesting(pelecanus_occidentalis, tree).
nesting(botaurus_lentiginosus, ground).
nesting(ixobrychus_exilis, ground).
nesting(ardea_herodias, tree).
nesting(ardea_alba, tree).
nesting(egretta_thula, tree).
nesting(egretta_caerulea, tree).
nesting(egretta_tricolor, tree).
nesting(egretta_rufescens, tree).
nesting(bubulcus_ibis, tree).
nesting(butorides_virescens, tree).
nesting(nycticorax_nycticorax, tree).
nesting(nyctanassa_violacea, tree).
nesting(eudocimus_albus, tree).
nesting(plegadis_falcinellus, ground).
nesting(plegadis_chihi, ground).
nesting(platalea_ajaja, tree).
nesting(X,Y) :- nonvar(X) -> isaStrict(S,X), nesting(S,Y).

food(pelecanus_erythrorhynchos, fish).
food(pelecanus_occidentalis, fish).
food(botaurus_lentiginosus, fish).
food(ixobrychus_exilis, fish).
food(ardea_herodias, fish).
food(ardea_alba, fish).
food(egretta_thula, fish).
food(egretta_caerulea, fish).
food(egretta_tricolor, fish).
food(egretta_rufescens, fish).
food(bubulcus_ibis, insects).
food(butorides_virescens, fish).
food(nycticorax_nycticorax, fish).
food(nyctanassa_violacea,  insects).
food(eudocimus_albus, insects).
food(plegadis_falcinellus, insects).
food(plegadis_chihi, insects).
food(platalea_ajaja, fish).
food(X,Y) :- nonvar(X) -> isaStrict(S,X), food(S,Y).

conservation(egretta_rufescens, nt).
conservation(X, lc) :- hasCompoundName(_,_,X).
conservation(X,Y) :- nonvar(X) -> isaStrict(S,X), conservation(S,Y).

%returns the behaviour associated with a given bird
behavior(X,Y):- var(X) -> hasCompoundName(_,_,X), behaves(X,Y).
behavior(X,Y):- atom(X) -> behaves(X,Y).

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
behaves(pelecanus, surfaceDive).
behaves(pelecanus, aerialDive).
behaves(pelecanidae, surfaceDive).
behaves(ardeidae, stalking).
behaves(ardeidae, groundForager).
behaves(botaurus, stalking).
behaves(ixobrychus, stalking).
behaves(ardea, stalking).
behaves(egretta, stalking).
behaves(bubulcus, groundForager).
behaves(butorides, stalking).
behaves(nycticorax, stalking).
behaves(nyctanassa, stalking).
behaves(threskiornithdae, probing).
behaves(eudocimus, probing).
behaves(plegadis, probing).
behaves(platalea, probing).
behaves(pelecaniformes, surfaceDive).
behaves(pelecaniformes, aerialDive).
behaves(pelecaniformes, probing).
behaves(pelecaniformes, stalking).
behaves(pelecaniformes, groundForager).


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
