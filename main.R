# Pacotes utilizados
install.packages(c("readxl", "dplyr", "ggplot2", "ggrepel"))

library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Carregar base
df <- read_excel("dados/DB_Cientometria_Expandida_ver01.xlsx")

# Criar pasta 'resultados' para salvar saídas, se não existir
if (!dir.exists("resultados")) {
  dir.create("resultados")
}

# 1) Número de artigos por ano
# Verificar quais estão com NA ou em branco no campo ano
artigos_sem_ano <- df %>%
  filter(is.na(year)) %>%
  select(title, doi, source_database, fascia)

artigos_sem_ano

write.csv(artigos_sem_ano, "resultados/artigos_sem_ano.csv", row.names = FALSE)

# Remover anos NA
df <- df %>% filter(!is.na(year))

# Análise dos dados restantes
artigos_ano <- df %>%
  group_by(year) %>%
  summarise(n_artigos = n())

artigos_ano %>% arrange(desc(n_artigos))

write.csv(artigos_ano, "resultados/artigos_por_ano.csv", row.names = FALSE)

# 2) Soma de citações por ano
# Verificar se possui citações em branco
citacoes_sem_dados <- df %>%
  filter(is.na(citation)) %>%
  select(title, doi, source_database, fascia)

citacoes_sem_dados

# Análise dos dados restantes
citacoes_ano <- df %>%
  group_by(year) %>%
  summarise(total_citacoes = sum(citations))

citacoes_ano %>% arrange(desc(total_citacoes))

write.csv(citacoes_ano, "resultados/total_citacoes.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# Mostrar:
# Como o impacto médio dos artigos evoluiu ao longo do tempo
# Se os artigos mais recentes estão sendo mais citados
# Se existe concentração de citações (alta variabilidade)

# Mas tem um problema estrutural:
# Artigos recentes (2024, 2025) ainda não tiveram tempo para acumular citações.

# Então:
# A média de citações por ano tende a cair nos anos mais recentes
# Não porque os artigos são piores
# Mas porque são mais novos
# Isso distorce análise temporal.

# Média e desvio padrão de citação por ano ***
estatisticas_citacao <- df %>%
  group_by(year) %>%
  summarise(
    media_score = mean(citations, na.rm = TRUE),
    desvio_padrao = sd(citations, na.rm = TRUE)
  )

write.csv(estatisticas_citacao, "resultados/estatisticas_citacao.csv", row.names = FALSE)

# Frequência por nível de importância por citação ***
importancia_citacao <- df %>%
  group_by(fascia) %>%
  summarise(n = n())

write.csv(importancia_citacao, "resultados/importancia_citacao.csv", row.names = FALSE)

importancia_citacao2 <- df %>%
  group_by(fascia) %>%
  summarise(n = n()) %>%
  mutate(percentual = (n / sum(n)) * 100)

write.csv(importancia_citacao2, "resultados/importancia_citacao2.csv", row.names = FALSE)

# ---------------------------------------------------------------------------

# 3) Média e desvio padrão do score por ano
estatisticas_score <- df %>%
  group_by(year) %>%
  summarise(
    media_score = mean(score_final_100, na.rm = TRUE),
    desvio_padrao = sd(score_final_100, na.rm = TRUE)
  )

write.csv(estatisticas_score, "resultados/estatisticas_score.csv", row.names = FALSE)

# 4) Número de artigos por base
artigos_base <- df %>%
  group_by(source_database) %>%
  summarise(n_artigos = n())

write.csv(artigos_base, "resultados/n_artigos_base.csv", row.names = FALSE)

# 5) Frequência por nível de importância
importancia <- df %>%
  group_by(fascia) %>%
  summarise(n = n())

write.csv(importancia, "resultados/importancia.csv", row.names = FALSE)

# ggplot(artigos_ano, aes(x = year, y = n_artigos)) +
#   geom_line() +
#   geom_point() +
#   labs(title = "Número de Artigos por Ano",
#        x = "Ano",
#        y = "Quantidade de Artigos") +
#   theme_minimal()


# ggplot(artigos_ano, aes(x = as.integer(year), y = n_artigos)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   scale_x_continuous(breaks = seq(min(artigos_ano$year),
#                                   max(artigos_ano$year), 
#                                   by = 2)) +
#   labs(title = "Evolução do Número de Artigos por Ano",
#        x = "Ano de Publicação",
#        y = "Quantidade de Artigos") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(artigos_ano, aes(x = year, y = n_artigos)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_point(data = subset(artigos_ano, year == 2025),
             color = "red", size = 4) +
  geom_text_repel(data = subset(artigos_ano, year == 2025),
                  aes(label = n_artigos),
                  nudge_y = 30) +
  scale_x_continuous(breaks = artigos_ano$year) +
  labs(title = "Evolução do Número de Artigos por Ano",
       x = "Ano de Publicação",
       y = "Quantidade de Artigos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(citacoes_ano, aes(x = year, y = total_citacoes)) +
  geom_line(size = 1, color = "darkred") +
  geom_point(size = 2, color = "darkred") +
  labs(title = "Total de Citações por Ano",
       x = "Ano",
       y = "Número de Citações") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(estatisticas_score, aes(x = year, y = media_score)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 2, color = "blue") +
  labs(title = "Média do Score Final por Ano",
       x = "Ano",
       y = "Média do Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(estatisticas_score, aes(x = year, y = desvio_padrao)) +
  geom_line(size = 1, color = "purple") +
  geom_point(size = 2, color = "purple") +
  labs(title = "Desvio Padrão do Score por Ano",
       x = "Ano",
       y = "Desvio Padrão") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(artigos_base, aes(x = reorder(source_database, n_artigos), y = n_artigos)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Número de Artigos por Base de Dados",
       x = "Base",
       y = "Quantidade de Artigos") +
  theme_minimal()

