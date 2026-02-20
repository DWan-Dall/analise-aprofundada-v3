# Pacotes utilizados
install.packages(c("readxl", "dplyr", "ggplot2", "ggrepel", "tidyr"))

library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)

# Carregar base
df <- read_excel("dados/DB_Cientometria_Expandida_ver01.xlsx")

# Criar pasta 'resultados' para salvar saídas, se não existir
if (!dir.exists("resultados")) {
  dir.create("resultados")
  dir.create("resultados/etapa-01")
  dir.create("resultados/etapa-02")
  dir.create("resultados/etapa-03")
  dir.create("resultados/etapa-04")
}

# 1) Número de artigos por ano
# Verificar quais estão com NA ou em branco no campo ano
artigos_sem_ano <- df %>%
  filter(is.na(year)) %>%
  select(title, doi, source_database, fascia)

artigos_sem_ano

write.csv(artigos_sem_ano, "resultados/etapa-01/artigos_sem_ano.csv", row.names = FALSE)

# Remover anos NA
df <- df %>% filter(!is.na(year))

# Análise dos dados restantes
artigos_ano <- df %>%
  group_by(year) %>%
  summarise(n_artigos = n())

artigos_ano %>% arrange(desc(n_artigos))

write.csv(artigos_ano, "resultados/etapa-01/artigos_por_ano.csv", row.names = FALSE)

ggplot(artigos_ano, aes(x = factor(year), y = n_artigos)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n_artigos),
            vjust = -0.5,
            size = 3.5) +
  labs(title = "Número de Artigos por Ano",
       x = "Ano de Publicação",
       y = "Quantidade de Artigos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path("resultados/etapa-01/artigos_por_ano_barras.jpg"),
  width = 12, height = 6
)

ano_max <- artigos_ano %>%
  filter(n_artigos == max(n_artigos)) %>%
  pull(year)

ggplot(artigos_ano, aes(x = factor(year), y = n_artigos)) +
  geom_col(aes(fill = year == ano_max), show.legend = FALSE) +
  scale_fill_manual(values = c("steelblue", "red")) +
  geom_text(aes(label = n_artigos),
            vjust = -0.5,
            size = 3.5) +
  labs(title = "Número de Artigos por Ano (Destaque para Maior Produção)",
       x = "Ano de Publicação",
       y = "Quantidade de Artigos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path("resultados/etapa-01/artigos_por_ano_barras_destaque.jpg"),
  width = 12, height = 6
)

ggplot(artigos_ano, aes(x = year, y = n_artigos)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_point(data = subset(artigos_ano, year == 2025),
             color = "red", size = 4) +
  geom_text_repel(data = subset(artigos_ano, year == 2025),
                  aes(label = n_artigos),
                  nudge_y = 30) +
  scale_x_continuous(breaks = artigos_ano$year) +
  labs(title = "Destaque do Ano de Maior Produção (2025)",
       x = "Ano de Publicação",
       y = "Quantidade de Artigos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path("resultados/etapa-01/artigos_por_ano_destaque.jpg"),
  width = 12, height = 6
)

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

write.csv(citacoes_ano, "resultados/etapa-02/total_citacoes.csv", row.names = FALSE)

ggplot(citacoes_ano, aes(x = factor(year), y = total_citacoes)) +
  geom_col(fill = "darkred") +
  geom_text(aes(label = total_citacoes),
            vjust = -0.2,
            size = 3.0) +
  labs(title = "Total de Citações por Ano",
       x = "Ano de Publicação",
       y = "Total de Citações") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path("resultados/etapa-02/citacoes_por_ano_barras.jpg"),
  width = 12, height = 6
)

ano_max_cit <- citacoes_ano %>%
  filter(total_citacoes == max(total_citacoes)) %>%
  pull(year)

ggplot(citacoes_ano, aes(x = factor(year), y = total_citacoes)) +
  geom_col(aes(fill = year == ano_max_cit), show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "red")) +
  geom_text(aes(label = total_citacoes),
            vjust = -0.5,
            size = 3.5) +
  labs(title = "Total de Citações por Ano (Destaque para Maior Impacto)",
       x = "Ano de Publicação",
       y = "Total de Citações") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path("resultados/etapa-02/citacoes_por_ano_barras_destaque_global.jpg"),
  width = 12, height = 6
)

ggplot(citacoes_ano, aes(x = year, y = total_citacoes)) +
  geom_line(size = 1, color = "darkred") +
  geom_point(size = 2, color = "darkred") +
  labs(title = "Total de Citações por Ano",
       x = "Ano",
       y = "Número de Citações") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path("resultados/etapa-02/citacoes_ano_grafico_global.jpg"),
  width = 12, height = 6
)


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

# ---------------------------------------------------------------------------

# Média e desvio padrão de citação por ano ***
estatisticas_citacao <- df %>%
  group_by(year) %>%
  summarise(
    media_score = mean(citations, na.rm = TRUE),
    desvio_padrao = sd(citations, na.rm = TRUE)
  )

estatisticas_citacao 

write.csv(estatisticas_citacao, "resultados/etapa-02/estatisticas_citacao_global.csv", row.names = FALSE)

# Frequência por nível de importância por citação ***
importancia_citacao <- df %>%
  group_by(fascia) %>%
  summarise(n = n())

importancia_citacao

write.csv(importancia_citacao, "resultados/etapa-02/importancia_citacao_global.csv", row.names = FALSE)

importancia_citacao2 <- df %>%
  group_by(fascia) %>%
  summarise(n = n()) %>%
  mutate(percentual = (n / sum(n)) * 100)

importancia_citacao2

write.csv(importancia_citacao2, "resultados/etapa-02/importancia_citacao2_global.csv", row.names = FALSE)

# Média de citações POR FASCIA

estatisticas_fascia <- df %>%
  group_by(fascia) %>%
  summarise(
    total_artigos = n(),
    media_citacoes = mean(citations, na.rm = TRUE),
    desvio_padrao = sd(citations, na.rm = TRUE)
  ) %>%
  arrange(desc(media_citacoes))

estatisticas_fascia

# group_by(fascia) %>%
# mutate(percentual = n / sum(n))

# ---------------
# Média de citações por artigo por ano
media_por_ano <- df %>%
  group_by(year) %>%
  summarise(
    total_artigos = n(),
    total_citacoes = sum(citations, na.rm = TRUE),
    media_citacoes = mean(citations, na.rm = TRUE)
  )

media_por_ano

# 2.1) Citações por ano_max

estatisticas_globais <- df %>%
  summarise(
    media_citacoes = mean(citations, na.rm = TRUE),
    desvio_padrao = sd(citations, na.rm = TRUE),
    total_artigos = n()
  )

estatisticas_globais

write.csv(estatisticas_globais,
          "resultados/etapa-02/estatisticas_globais_citacoes.csv",
          row.names = FALSE)


# ano_max <- artigos_ano %>%
#   filter(n_artigos == max(n_artigos)) %>%
#   pull(year)

ano_max

estatisticas_ano_max <- df %>%
  filter(year == ano_max) %>%
  summarise(
    media_citacoes = mean(citations, na.rm = TRUE),
    desvio_padrao = sd(citations, na.rm = TRUE),
    total_artigos = n()
  )

estatisticas_ano_max

write.csv(estatisticas_ano_max,
          "resultados/etapa-02/estatisticas_citacoes_ano_max.csv",
          row.names = FALSE)

# ---------------
# Se acontecer algo como:
# Ano com mais publicações tem média menor → pode indicar produção em massa com menor impacto individual.
# Ano com mais publicações tem média maior → indica consolidação e maturidade do campo.
# Isso vira argumento de discussão.


comparacao <- bind_rows(
  estatisticas_globais %>% mutate(grupo = "Base Completa"),
  estatisticas_ano_max %>% mutate(grupo = paste("Ano", ano_max))
)


ggplot(comparacao, aes(x = grupo, y = media_citacoes)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(media_citacoes, 2)),
            vjust = -0.5,
            size = 4) +
  labs(title = "Comparação da Média de Citações",
       x = "",
       y = "Média de Citações") +
  theme_minimal()

ggsave("resultados/etapa-02/comparacao_media_citacoes.jpg",
       width = 12, height = 6)


estatisticas_long <- estatisticas_ano_max %>%
  select(media_citacoes, desvio_padrao) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "metrica",
                      values_to = "valor")


ggplot(estatisticas_long, aes(x = metrica, y = valor)) +
  geom_col(fill = "darkred") +
  geom_text(aes(label = round(valor, 2)),
            vjust = -0.5,
            size = 4) +
  labs(title = paste("Estatísticas de Citação - Ano", ano_max),
       x = "",
       y = "Valor") +
  theme_minimal()
# --------------------
top_artigos <- df %>%
  arrange(desc(citations)) %>%
  select(title, year, citations, source_database, fascia) %>%
  head(20)

top_artigos

write.csv(top_artigos,
          "resultados/etapa-02/top_20_artigos_mais_citados.csv",
          row.names = FALSE)


ggplot(top_artigos,
       aes(x = reorder(title, citations),
           y = citations)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Top 20 Artigos Mais Citados",
       x = "",
       y = "Número de Citações") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

media_por_ano <- df %>%
  group_by(year) %>%
  summarise(media_citacoes = mean(citations, na.rm = TRUE))

media_por_ano

#---------------------

# 3) Média e desvio padrão do score por ano
# Score temático, aderência a "flood prediction" + "AI/ML"
estatisticas_score_t <- df %>%
  group_by(year) %>%
  summarise(
    media_score = mean(T_score, na.rm = TRUE),
    desvio_padrao = sd(T_score, na.rm = TRUE)
  )

estatisticas_score_t

write.csv(estatisticas_score_t, "resultados/etapa-03/estatisticas_score_tematico.csv", row.names = FALSE)

ggplot(estatisticas_score_t, aes(x = year, y = media_score)) +
  geom_line(size = 1, color = "pink") +
  geom_point(size = 2, color = "pink") +
  labs(title = "Média do Score Temático",
       x = "Ano",
       y = "Média do Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path("resultados/etapa-03/estatisticas_score_tematico_grafico.jpg"),
  width = 12, height = 6
)

ggplot(estatisticas_score_t, aes(x = year, y = desvio_padrao)) +
  geom_line(size = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  labs(title = "Desvio Padrão do Score Temático por Ano",
       x = "Ano",
       y = "Desvio Padrão") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path("resultados/etapa-03/estatisticas_score_tematico_desvio_padrao_grafico.jpg"),
  width = 12, height = 6
)

# Score temático, aderência a "flood prediction" + "AI/ML"
estatisticas_score_t <- df %>%
  group_by(year) %>%
  summarise(
    media_score = mean(T_score, na.rm = TRUE),
    desvio_padrao = sd(T_score, na.rm = TRUE)
  )

estatisticas_score_t

write.csv(estatisticas_score_t, "resultados/etapa-03/estatisticas_score_tematico.csv", row.names = FALSE)

ggplot(estatisticas_score_t, aes(x = year, y = media_score)) +
  geom_line(size = 1, color = "pink") +
  geom_point(size = 2, color = "pink") +
  labs(title = "Média do Score Temático",
       x = "Ano",
       y = "Média do Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path("resultados/etapa-03/estatisticas_score_tematico_grafico.jpg"),
  width = 12, height = 6
)

ggplot(estatisticas_score_t, aes(x = year, y = desvio_padrao)) +
  geom_line(size = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  labs(title = "Desvio Padrão do Score Temático por Ano",
       x = "Ano",
       y = "Desvio Padrão") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path("resultados/etapa-03/estatisticas_score_tematico_desvio_padrao_grafico.jpg"),
  width = 12, height = 6
)

estatisticas_score_final_100 <- df %>%
  group_by(year) %>%
  summarise(
    media_score = mean(score_final_100, na.rm = TRUE),
    desvio_padrao = sd(score_final_100, na.rm = TRUE)
  )

estatisticas_score_final_100

write.csv(estatisticas_score_final_100, "resultados/etapa-03/estatisticas_score_final_100.csv", row.names = FALSE)

ggplot(estatisticas_score_final_100, aes(x = year, y = media_score)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 2, color = "blue") +
  labs(title = "Média do Score Final por Ano",
       x = "Ano",
       y = "Média do Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path("resultados/etapa-03/estatisticas_score_media_grafico.jpg"),
  width = 12, height = 6
)

ggplot(estatisticas_score_final_100, aes(x = year, y = desvio_padrao)) +
  geom_line(size = 1, color = "purple") +
  geom_point(size = 2, color = "purple") +
  labs(title = "Desvio Padrão do Score por Ano",
       x = "Ano",
       y = "Desvio Padrão") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path("resultados/etapa-03/estatisticas_score_desvio_padrao_grafico.jpg"),
  width = 12, height = 6
)

# 4) Número de artigos por base
artigos_base <- df %>%
  separate_rows(source_database, sep = ";") %>%
  mutate(source_database = trimws(source_database)) %>% 
  group_by(source_database) %>%
  summarise(n_artigos = n()) %>%
  arrange(desc(n_artigos))

artigos_base

write.csv(artigos_base, "resultados/etapa-04/n_artigos_base.csv", row.names = FALSE)


ggplot(artigos_base,
       aes(x = reorder(source_database, n_artigos),
           y = n_artigos)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = n_artigos),
            hjust = -0.1,
            size = 3.5) +
  labs(title = "Número de Artigos por Base de Dados",
       x = "Base",
       y = "Quantidade de Artigos") +
  theme_minimal()

ggsave(
  file.path("resultados/etapa-04/n_artigos_base_grafico.jpg"),
  width = 12, height = 6
)


# 5) Frequência por nível de importância
importancia <- df %>%
  group_by(fascia) %>%
  summarise(n = n())

importancia

write.csv(importancia, "resultados/etapa-04/importancia.csv", row.names = FALSE)




