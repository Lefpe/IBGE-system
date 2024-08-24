import Data.List (sort)
import System.IO (hFlush, stdout)

-- Definição do tipo 'Pessoa'
data Pessoa = Pessoa {
    nome :: String,
    idade :: Int,
    endereco :: Endereco
} deriving (Show, Eq)

-- Definição do tipo 'Endereco'
data Endereco = Endereco {
    rua :: String,
    casa :: String,
    cidade :: String
} deriving (Show, Eq)

-- Função para comparar pessoas pelo nome (usada para ordenar a lista)
instance Ord Pessoa where
    compare p1 p2 = compare (nome p1) (nome p2)

-- Função que insere ou atualiza uma pessoa na lista
inserirOuAtualizarPessoa :: [Pessoa] -> Pessoa -> [Pessoa]
inserirOuAtualizarPessoa pessoas novaPessoa =
    let semPessoaAntiga = filter (\p -> nome p /= nome novaPessoa) pessoas
    in sort (novaPessoa : semPessoaAntiga)

-- Função que localiza uma pessoa pelo nome
localizarPessoa :: [Pessoa] -> String -> Maybe Pessoa
localizarPessoa [] _ = Nothing
localizarPessoa (p:ps) nomeBuscado
    | nome p == nomeBuscado = Just p
    | otherwise             = localizarPessoa ps nomeBuscado

-- Função que conta o número de pessoas em uma cidade específica
totalPorCidade :: [Pessoa] -> String -> Int
totalPorCidade pessoas cidadeBuscada =
    length (filter (\p -> cidade (endereco p) == cidadeBuscada) pessoas)

-- Função que calcula a média de idade da população
mediaIdade :: [Pessoa] -> Double
mediaIdade pessoas =
    let totalIdade = sum (map idade pessoas)
        totalPessoas = length pessoas
    in if totalPessoas == 0
       then 0
       else fromIntegral totalIdade / fromIntegral totalPessoas

-- Função para inserir uma nova pessoa solicitando os dados ao usuário
novaPessoa :: IO Pessoa
novaPessoa = do
    putStr "Digite o nome: "
    hFlush stdout
    nomePessoa <- getLine

    putStr "Digite a idade: "
    hFlush stdout
    idadeStr <- getLine
    let idadePessoa = read idadeStr :: Int

    putStr "Digite a rua: "
    hFlush stdout
    ruaPessoa <- getLine

    putStr "Digite a casa: "
    hFlush stdout
    casaPessoa <- getLine

    putStr "Digite a cidade: "
    hFlush stdout
    cidadePessoa <- getLine

    putStrLn "Pessoa cadastrada com sucesso!\n"
    return $ Pessoa nomePessoa idadePessoa (Endereco ruaPessoa casaPessoa cidadePessoa)

-- Função que exibe a lista de pessoas cadastradas
verListaPessoas :: [Pessoa] -> IO ()
verListaPessoas [] = putStrLn "Nenhuma pessoa cadastrada.\n"
verListaPessoas pessoas = do
    putStrLn "\nLista de Pessoas Cadastradas:"
    mapM_ print pessoas
    putStrLn ""

-- Função que exibe o número de pessoas cadastradas
verNumeroPessoas :: [Pessoa] -> IO ()
verNumeroPessoas pessoas = do
    putStrLn $ "\nNúmero de pessoas cadastradas: " ++ show (length pessoas) ++ "\n"

-- Função para buscar uma pessoa pelo nome
buscarPessoa :: [Pessoa] -> IO ()
buscarPessoa pessoas = do
    putStr "Digite o nome da pessoa que deseja buscar: "
    hFlush stdout
    nomeBuscado <- getLine
    case localizarPessoa pessoas nomeBuscado of
        Just pessoa -> print pessoa
        Nothing     -> putStrLn "Pessoa não encontrada."
    putStrLn ""

-- Função para ver o número de pessoas em uma cidade
verNumeroPorCidade :: [Pessoa] -> IO ()
verNumeroPorCidade pessoas = do
    putStr "Digite o nome da cidade: "
    hFlush stdout
    cidadeBuscada <- getLine
    let total = totalPorCidade pessoas cidadeBuscada
    putStrLn $ "Número de pessoas na cidade " ++ cidadeBuscada ++ ": " ++ show total
    putStrLn ""

-- Função para ver a média de idade
verMediaIdade :: [Pessoa] -> IO ()
verMediaIdade pessoas = do
    let media = mediaIdade pessoas
    putStrLn $ "Média de idade dos cadastrados: " ++ show media
    putStrLn ""

-- Função para ver a média da população (total da média de idade de todos os cadastrados)
verMediaPopulacao :: [Pessoa] -> IO ()
verMediaPopulacao pessoas = do
    let media = mediaIdade pessoas
    putStrLn $ "Média da população: " ++ show media
    putStrLn ""

-- Função que exibe o menu e gerencia as escolhas do usuário
menu :: [Pessoa] -> IO ()
menu pessoas = do
    putStrLn "=== Sistema de Cadastro de Pessoas ==="
    putStrLn "1. Cadastrar nova pessoa"
    putStrLn "2. Ver lista de pessoas cadastradas"
    putStrLn "3. Ver número de pessoas cadastradas"
    putStrLn "4. Buscar pessoa pelo nome"
    putStrLn "5. Ver número de pessoas em uma cidade"
    putStrLn "6. Ver média de idade dos cadastrados"
    putStrLn "7. Ver média da população"
    putStrLn "8. Sair"
    putStr "Escolha uma opção: "
    hFlush stdout
    opcao <- getLine
    putStrLn ""
    case opcao of
        "1" -> do
            pessoa <- novaPessoa
            let novasPessoas = inserirOuAtualizarPessoa pessoas pessoa
            menu novasPessoas
        "2" -> do
            verListaPessoas pessoas
            menu pessoas
        "3" -> do
            verNumeroPessoas pessoas
            menu pessoas
        "4" -> do
            buscarPessoa pessoas
            menu pessoas
        "5" -> do
            verNumeroPorCidade pessoas
            menu pessoas
        "6" -> do
            verMediaIdade pessoas
            menu pessoas
        "7" -> do
            verMediaPopulacao pessoas
            menu pessoas
        "8" -> putStrLn "Saindo do sistema. Até mais!"
        _   -> do
            putStrLn "Opção inválida. Tente novamente.\n"
            menu pessoas

-- Função principal
main :: IO ()
main = menu []
