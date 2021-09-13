-- contato = (nome, endereço, telefone, email, relação)
type Contact = ([Char],[Char],[Char],[Char], [Char])
type Schedule = [Contact]

contactList::Schedule
contactList = [("Ana","Rua A","(22)91234-1234","ana@email.com", "irma"),
    ("Clara","Rua B","(22)92341-2341","clara@email.com", "amiga"),
    ("Jose","Rua C","(22)93423-3423","jose@email.com", "avo"),
    ("Silvio","Rua A","(22)92134-2134","silvio@email.com", "pai"),
    ("Marta","Rua A","(22)94213-4213","marta@email.com", "prima")]

listContacts = do
    print $ contactList

findContact::[Char]->Schedule->[Char]
findContact x [] = "Contato nao encontrado"
findContact x ((n,a,p,e,r):cs) | x == n = n ++ " - " ++ a ++ " - " ++ p ++ " - " ++ e ++ " - " ++ r
    | otherwise = findContact x cs

addContact::Contact->Schedule
addContact c = c : contactList