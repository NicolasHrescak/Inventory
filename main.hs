import qualified Data.Map as Map
import Data.Time.Clock (UTCTime)
import Data.List (isInfixOf)
import System.IO (IO, FilePath, writeFile, appendFile, readFile, putStrLn, getLine, putStr, hFlush, stdout)
import Control.Exception (catch, IOException)
import System.IO.Error (isDoesNotExistError)

-- Tipos para o item do inventário conforme o documento:
data Item = Item {
    itemID :: String,
    nome :: String,
    quantidade :: Int,
    categoria :: String
} deriving (Show, Read) -- Aqui definimos a derivação de show e read 

-- Tipo do inventário como Map String Item 
type Inventario = Map.Map String Item

-- AcaoLog no formato mencionado no documento
data AcaoLog = Add | Remove | Update | QueryFail -- Define quais respostas ele pode ter
    deriving (Show, Read) -- também derivamos de show e read
    
-- StatusLog no formato mencionado
data StatusLog = Sucesso | Falha String -- Define quais respostas ele pode ter
    deriving (Show, Read) -- derivado de show e read 
    
-- Tipo de entrada do Log com todos os tipos de dado mencionados
data LogEntry = LogEntry {
    timestamp :: UTCTime, 
    acao :: AcaoLog,
    detalhes :: String,
    status :: StatusLog
} deriving (Show, Read) -- derivando de show e read

-- Definicao do tipo de retorno de sucesso 
type ResultadoOperacao = (Inventario, LogEntry)

--------------------------------------------------------------------------------------------------------
--------------- FUNCAO PARA ADICIONAR UM NOVO ITEM 
additem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
additem timestamp itemID nome qtd categoria inventario = -- Nome pra especificar os parametros da funcao
    case Map.lookup itemID inventario of -- Percorre por itemID no map inventário
        -- A funcao FALHA se o ID ja existir (permitindo apenas um único ID por item)
        Just _ -> Left "Erro: Item com este ID ja existe."
        
        -- Se o ID não existir, sucesso e prossegue
        Nothing ->
            let
                -- Variável pra criar um novo item
                novoItem = Item {
                    itemID = itemID,
                    nome = nome,
                    quantidade = qtd,
                    categoria = categoria
                }
                
                -- Adiciona o item ao inventario
                novoInventario = Map.insert itemID novoItem inventario
                
                -- Cria a mensagem do log
                detalhes = "Item adicionado: " ++ nome ++ " (ID: " ++ itemID ++ ")"
                
                -- Cria a entrada de log de sucesso
                logEntry = LogEntry {
                    timestamp = timestamp,
                    acao = Add,
                    detalhes = detalhes,
                    status = Sucesso
                }
            in
                -- Retorna o novo estado e o log de sucesso da funcao
                Right (novoInventario, logEntry)
                
--------------------------------------------------------------------------------------------------------
--------------- FUNCAO PARA REMOVER UM ITEM 
removeltem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeltem timestamp itemID qtdRemover inventario =
    case Map.lookup itemID inventario of
        -- Falha
        Nothing -> Left "Erro: Item nao encontrado para remocao."
        
        -- Sucesso, encontrou o item para remover
        Just itemEncontrado ->
            if quantidade itemEncontrado < qtdRemover then -- Se a quantidade de remover por maior == Falha
                -- Falha, Estoque insuficiente
                Left ("Erro: Estoque insuficiente para " ++ nome itemEncontrado ++ ". Estoque disponivel: " ++ show (quantidade itemEncontrado))
            else
                -- Sucesso, Estoque suficiente
                let
                    -- Atualiza   a quantidade do item
                    itemAtualizado = itemEncontrado { 
                        quantidade = quantidade itemEncontrado - qtdRemover 
                    }
                    
                    -- Atualiza o inventário com a nova quantidade de item
                    novoInventario = Map.insert itemID itemAtualizado inventario
                    
                    -- Cria o log
                    detalhes = "Removido " ++ show qtdRemover ++ " unidades do item " ++ nome itemEncontrado ++ " (ID: " ++ itemID ++ ")"
                    
                    -- Cria o de log de sucesso
                    logEntry = LogEntry {
                        timestamp = timestamp,
                        acao = Remove,
                        detalhes = detalhes,
                        status = Sucesso
                    }
                in
                    -- Retorna o novo estado e o log de sucesso
                    Right (novoInventario, logEntry)

--------------------------------------------------------------------------------------------------------
--------------- FUNCAO PARA ATUALIZAR A QUANTIDADE DE UM ITEM 
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty timestamp itemID qtdAdicionar inventario =
    case Map.lookup itemID inventario of
        -- Falha
        Nothing -> Left "Erro: Item nao encontrado para atualizar estoque."
        
        -- Encontrou o item 
        Just itemEncontrado ->
            if qtdAdicionar <= 0 then
                -- Se tentar adicionar 0 ou valor negativo cai aqui
                Left "Erro: Quantidade a adicionar deve ser positiva."
            else
                -- Quando o valor é válido:
                let
                    itemAtualizado = itemEncontrado {
                        quantidade = quantidade itemEncontrado + qtdAdicionar
                    }
                    
                    novoInventario = Map.insert itemID itemAtualizado inventario
                    
                    detalhes = "Adicionado ao estoque " ++ show qtdAdicionar ++ " unidades do item " ++ nome itemEncontrado ++ " (ID: " ++ itemID ++ ")"
                    
                    logEntry = LogEntry {
                        timestamp = timestamp,
                        acao = Update,
                        detalhes = detalhes,
                        status = Sucesso
                    }
                in
                    Right (novoInventario, logEntry)

inventarioFile :: FilePath -- Arquivo que persiste o estado atual 
inventarioFile = "Inventario.dat"

logFile :: FilePath -- Arquivo de log 
logFile = "Auditoria.log"

-- Função para carregar o inventario 
carregarInventario :: IO Inventario
carregarInventario = (do
    -- Tenta ler o arquivo
    conteudo <- readFile inventarioFile
    -- O reasd converte a string de volta para o tipo Inventario
    let inventario = read conteudo
    putStrLn "Inventario.dat carregado."
    return inventario
    ) `catch` \e -> do -- Catch para lidar com ausencia de arquivo conforme o documento pede 
        if isDoesNotExistError e then do
            putStrLn "[INFO] Inventario.dat nao encontrado. Iniciando com inventario vazio."
            return Map.empty -- Se não encontrar inicia com o inventario vazio
        else
            ioError e -- Verifica outros erros de IO possíveis
            
-- Função pra tentar ler o auditoria.log na inicialização
carregarLog :: IO [LogEntry]
carregarLog = (do
    conteudo <- readFile logFile
    
    -- Se o arquivo estiver vazio o 'lines' retorna [], o que é uma medida de segurança.
    let linhas = lines conteudo
    -- Usamos 'map read' para converter cada linha (string) em um LogEntry
    let logs = map read linhas :: [LogEntry]
    putStrLn "Auditoria.log carregado."
    return logs
    
    ) `catch` \e -> do
        if isDoesNotExistError e then do -- Se não encontrar inicia com o log vazio conforme o documento
            putStrLn "Auditoria.log nao encontrado. Iniciando com log vazio."
            return [] -- Estado vazio
        else
            ioError e
            
-- Funções de persistencia 
salvarInventario :: Inventario -> IO () -- Sempre reescrevendo o arquivo Inventario a cada operacao 
salvarInventario inv = writeFile inventarioFile (show inv) -- bem sucedida 

-- Registrando as tentativas no modo append-only
salvarLog :: LogEntry -> IO ()
salvarLog logEntry = appendFile logFile (show logEntry ++ "\n")


main :: IO ()
main = do
    putStrLn "Iniciando Sistema de Inventario em Haskell"
    -- Carrega os estados iniciais dos arquivos chamando as funções
    inventarioInicial <- carregarInventario
    logsIniciais <- carregarLog
    
    -- Inicia o loop de execução com os estados carregados
    mainLoop inventarioInicial logsIniciais
    
    
mainLoop :: Inventario -> [LogEntry] -> IO ()
mainLoop inventario logs = do
    -- Exibição do menu e dos comandos
    putStrLn "\n--- Sistema de Inventario ---"
    putStrLn "Comandos: add, remove, update, list, report, quit"
    putStr "Digite um comando: "
    hFlush stdout -- Garante que o print apareça antes do input
    input <- getLine
    let parts = words input -- Separa o input
    
    -- Pega o horário atual
    agora <- getCurrentTime

    -- Chamando as funções de acordo com a opção escolhida do usuário
    case parts of
        ["add"] -> do
            -- Coleta de dados necessários para adicionar 
            putStr "ID: "; hFlush stdout; id <- getLine
            putStr "Nome: "; hFlush stdout; nome <- getLine
            putStr "Quantidade: "; hFlush stdout; qtdStr <- getLine
            putStr "Categoria: "; hFlush stdout; cat <- getLine
            
            -- Chamada da função de adicionar 
            let resultado = additem agora id nome (read qtdStr) cat inventario
            
            -- Processamento do resultado
            case resultado of
                -- Se bem sucedida escreve e adiciona no inventário
                Right (novoInv, logEntry) -> do
                    salvarInventario novoInv
                    salvarLog logEntry
                    putStrLn "Item adicionado ao inventario."
                    mainLoop novoInv (logEntry : logs) -- Continua o loop com o novo estado
                
                -- Se falhar chama apenas o log para escrever
                Left erroMsg -> do
                    putStrLn $ "Ocorreu uma falha" ++ erroMsg
                    let logEntryFalha = LogEntry agora Add ("Tentativa de add: " ++ id) (Falha erroMsg)
                    salvarLog logEntryFalha
                    mainLoop inventario (logEntryFalha : logs) -- Continua o loop com o estado antigo

        ["remove"] -> do
            -- Coleta de dados
            putStr "ID do item: "; hFlush stdout; id <- getLine
            putStr "Qtd a remover: "; hFlush stdout; qtdStr <- getLine
            
            -- Chama a função
            let resultado = removeltem agora id (read qtdStr) inventario
            
            -- Processamento do resultado
            case resultado of
                Right (novoInv, logEntry) -> do
                    salvarInventario novoInv
                    salvarLog logEntry
                    putStrLn "Quantidade removida do item."
                    mainLoop novoInv (logEntry : logs)
                
                Left erroMsg -> do
                    putStrLn $ " Falha " ++ erroMsg
                    -- Caso de tentastiva de remover mais do que existe
                    let logEntryFalha = LogEntry agora Remove ("Tentativa de remover: " ++ id) (Falha erroMsg)
                    salvarLog logEntryFalha
                    mainLoop inventario (logEntryFalha : logs)

        ["update"] -> do
            -- Coleta de dados
            putStr "ID do item: "; hFlush stdout; id <- getLine
            putStr "Quantidade a adicionar: "; hFlush stdout; qtdStr <- getLine
            
            -- Chamada da função de update
            let resultado = updateQty agora id (read qtdStr) inventario
            
            -- Processamento do resultado
            case resultado of
                Right (novoInv, logEntry) -> do
                    salvarInventario novoInv
                    salvarLog logEntry
                    putStrLn "Estoque do item atualizado."
                    mainLoop novoInv (logEntry : logs)
                
                Left erroMsg -> do
                    putStrLn $ "Falha " ++ erroMsg
                    let logEntryFalha = LogEntry agora Update ("Tentativa de update: " ++ id) (Falha erroMsg)
                    salvarLog logEntryFalha
                    mainLoop inventario (logEntryFalha : logs)

        ["list"] -> do
            -- Comando de listar
            putStrLn "\n--- Inventario Atual ---"
            print inventario -- 'print' é 'show' + 'putStrLn'
            mainLoop inventario logs -- Continua com o mesmo estado

        ["report"] -> do
            -- "Comando de Relatório" Será implementado na Etapa 4
            putStrLn "Funcao de relatorio (Etapa 4) sera chamada aqui."
            -- Por enquanto, apenas voltamos ao loop
            mainLoop inventario logs
        
        ["quit"] ->
            putStrLn "Encerrando sistema."

        -- Qualquer comando inválido
        _ -> do
            putStrLn "Falhou, comando nao reconhecido."
            let logEntryFalha = LogEntry agora QueryFail ("Comando invalido: " ++ input) (Falha "Comando nao reconhecido")
            salvarLog logEntryFalha
            mainLoop inventario (logEntryFalha : logs)