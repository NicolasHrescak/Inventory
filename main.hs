import qualified Data.Map as Map
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.List (isInfixOf)
import System.IO (IO, FilePath, writeFile, appendFile, readFile, putStrLn, getLine, putStr, hFlush, stdout)
import Control.Exception (catch, IOException, evaluate)
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)

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
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem timestamp itemID qtdRemover inventario =
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
    conteudo <- readFile inventarioFile -- Serve para o codigo sempre ler o arquivo
    
    _ <- evaluate (length conteudo)
    -- O reasd converte a string de volta para o tipo Inventario
    let inventario = read conteudo
    putStrLn "Inventario.dat carregado."
    return inventario
    ) `catch` \e -> do -- Catch para lidar com ausencia de arquivo conforme o documento pede 
        if isDoesNotExistError e then do
            putStrLn "Inventario.dat nao encontrado. Iniciando com inventario vazio."
            return Map.empty -- Se não encontrar inicia com o inventario vazio
        else
            ioError e -- Verifica outros erros de IO possíveis
            
-- Função pra tentar ler o auditoria.log na inicialização
carregarLog :: IO [LogEntry]
carregarLog = (do
    conteudo <- readFile logFile
    
    _ <- evaluate (length conteudo) -- Serve para o codigo sempre ler o arquivo
    
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


-- Função log de erro
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro logs = filter ehLogDeErro logs
  where
    ehLogDeErro :: LogEntry -> Bool
    ehLogDeErro log =
      case status log of
        Falha _ -> True  -- Se o status for 'Falha', mantém
        Sucesso -> False -- Se for 'Sucesso', é descartada
        
-- Função de histórico por item
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem idProcurado logs = filter (contemItemID idProcurado) logs
  where
    contemItemID :: String -> LogEntry -> Bool
    contemItemID id log =
        let idString = "(ID: " ++ id ++ ")"
        in idString `isInfixOf` (detalhes log)
        
        
-- Função para o relatorio
relatorioSimplesMovimentacao :: [LogEntry] -> String
relatorioSimplesMovimentacao logs =
    "Total de operacoes de Sucesso (Add/Remove/Update): " ++ show (length movimentos) ++ "\n" ++
    "Total de Falhas: " ++ show (length (logsDeErro logs))
  where
    movimentos = filter ehMovimento logs
    ehMovimento log =
        case (acao log, status log) of
            (Add, Sucesso) -> True
            (Remove, Sucesso) -> True
            (Update, Sucesso) -> True
            _ -> False

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
            
            let maybeQtd = readMaybe qtdStr :: Maybe Int
            
            case maybeQtd of
                -- Falha a entrada não era um número
                Nothing -> do
                    putStrLn "Quantidade invalida. Por favor, insira um numero."
                    let logEntryFalha = LogEntry agora Add ("Tentativa de add: " ++ id) (Falha "Input invalido para quantidade")
                    salvarLog logEntryFalha
                    mainLoop inventario (logEntryFalha : logs) -- Volta ao loop
                
                -- O valor é um número
                Just qtd -> do
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
            
            let maybeQtd = readMaybe qtdStr :: Maybe Int
            
            case maybeQtd of
                -- input não numérico
                Nothing -> do
                    putStrLn "Quantidade invalida. Por favor, insira um numero."
                    let logEntryFalha = LogEntry agora Remove ("Tentativa de remove: " ++ id) (Falha "Input invalido para quantidade")
                    salvarLog logEntryFalha
                    mainLoop inventario (logEntryFalha : logs)
                
                Just qtdRemover -> do
            
                    -- Chama a função
                    let resultado = removeItem agora id (read qtdStr) inventario
                    
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
            
            let maybeQtd = readMaybe qtdStr :: Maybe Int
            
            case maybeQtd of
                -- input não numérico
                Nothing -> do
                    putStrLn "Quantidade invalida. Por favor, insira um numero."
                    let logEntryFalha = LogEntry agora Update ("Tentativa de update: " ++ id) (Falha "Input invalido para quantidade")
                    salvarLog logEntryFalha
                    mainLoop inventario (logEntryFalha : logs)
                
                -- input é numérico
                Just qtdAdicionar -> do
            
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
            
            -- Pega todos os itens e transforma numa lista de items APENAS PARA FICAR SEPARADO NO PRINT
            let listaDeItens = Map.elems inventario
            
            -- Verifica se o inventário está vazio
            if null listaDeItens
            then putStrLn "O inventario esta vazio."
            else 
                -- O mapM executa um print pra cada um da list, assim pulando linha e ficando corretamente no print 
                mapM_ print listaDeItens
                
            mainLoop inventario logs -- Continua com o mesmo estado
            
            

        ["report"] -> do
            putStrLn "\n--- Gerando Relatorios ---"

            -- Relatório de erro
            let erros = logsDeErro logs
            putStrLn "--- Relatorio de Erros ---"
            if null erros
            then putStrLn "Nenhum erro registrado."
            else mapM_ (putStrLn . show) erros -- 'mapM_' aplica 'print' a cada item
    
            -- Relatório de histórico 
            putStrLn "\n--- Consultar Historico por Item ---"
            putStr "Digite o ID do item para ver historico (ou deixe em branco): "; hFlush stdout
            idConsulta <- getLine
    
            if null idConsulta
            then putStrLn "Consulta pulada."
            else do
                let historico = historicoPorItem idConsulta logs
                mapM_ (putStrLn . show) historico
    
            -- Relatório de Movimentação
            putStrLn "\n--- Relatorio de Movimentacao ---"
            putStrLn $ relatorioSimplesMovimentacao logs
    
            putStrLn "--- Fim dos Relatorios ---"
            mainLoop inventario logs -- Retorna ao loop
        
        ["quit"] ->
            putStrLn "Encerrando sistema."

        -- Qualquer comando inválido
        _ -> do
            putStrLn "Falhou, comando nao reconhecido."
            let logEntryFalha = LogEntry agora QueryFail ("Comando invalido: " ++ input) (Falha "Comando nao reconhecido")
            salvarLog logEntryFalha
            mainLoop inventario (logEntryFalha : logs)
