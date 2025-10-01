# -------------------------
# Pacotes
library(httr)
library(jsonlite)
library(dplyr)
library(readr) # Necessário para ler e escrever arquivos CSV

# -------------------------
# Caminho do arquivo para salvar os dados
output_file <- "artistas_relacionados.csv"

# -------------------------
# Função para adicionar dados ao arquivo CSV
adicionar_ao_csv <- function(data, file_path) {
  # Verifica se o arquivo já existe
  if (file.exists(file_path)) {
    # Lê o arquivo existente para um data frame
    existing_df <- read_csv(file_path)
    
    # Combina os data frames
    combined_df <- bind_cols(existing_df, data)
    
    # Salva o data frame combinado de volta no arquivo
    write_csv(combined_df, file_path)
    
  } else {
    # Se o arquivo não existir, cria um novo com os dados atuais
    write_csv(data, file_path)
  }
}

# -------------------------
# Entrada do usuário
artist_name_input <- readline(prompt = "Digite o nome do artista que você deseja encontrar (ex: Queen): ")

# -------------------------
# Buscar artista na API do Deezer
search_url <- paste0(
  "https://api.deezer.com/search/artist?q=",
  URLencode(artist_name_input)
)

search_response <- GET(search_url)

cat("search status:", status_code(search_response), "\n")

search_content <- content(search_response, as = "text", encoding = "UTF-8")
search_list <- fromJSON(search_content)

artist_id <- NULL
artist_name_found <- NA

# Extrair o ID do artista da resposta da busca
if (!is.null(search_list$data) && nrow(search_list$data) > 0) {
  artist_id <- search_list$data$id[1]
  artist_name_found <- search_list$data$name[1]
}

# -------------------------
# Validar ID e buscar artistas relacionados
if (is.null(artist_id) || is.na(artist_id)) {
  cat("Artista não encontrado ou ID não extraído.\n")
} else {
  cat("Artista encontrado:", artist_name_found, "| ID:", artist_id, "\n")
  
  related_url <- paste0(
    "https://api.deezer.com/artist/",
    artist_id,
    "/related"
  )
  
  related_response <- GET(related_url)
  
  cat("related status:", status_code(related_response), "\n")
  
  if (status_code(related_response) == 200) {
    related_content <- content(related_response, as = "text", encoding = "UTF-8")
    related_list <- fromJSON(related_content)
    
    if (!is.null(related_list$data) && nrow(related_list$data) > 0) {
      related_df <- related_list$data
      
      # Pegar apenas os 20 primeiros artistas
      final_df <- head(related_df, 20)
      
      # Selecionar apenas o nome e transformar em um data frame
      # A coluna terá o nome do artista pesquisado
      final_df <- data.frame(final_df$name)
      colnames(final_df) <- artist_name_found
      
      # Adicionar o novo data frame ao arquivo CSV
      adicionar_ao_csv(final_df, output_file)
      
      cat("\nResultados adicionados com sucesso ao arquivo:", output_file, "\n")
      print(final_df)
      
    } else {
      cat("Nenhum artista relacionado encontrado.\n")
    }
  } else {
    cat("Erro na requisição de artistas relacionados:", status_code(related_response), "\n")
    cat(content(related_response, "text", encoding = "UTF-8"), "\n")
  }
}