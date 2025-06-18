import os

cartella = "./R"  # o "/percorso/assoluto/R"
output_file = "unione.R"

with open(output_file, "w", encoding="utf-8") as unione:
    for nome_file in sorted(os.listdir(cartella)):
        if nome_file.endswith(".R"):
            percorso = os.path.join(cartella, nome_file)
            with open(percorso, "r", encoding="utf-8") as singolo:
                unione.write(f"# Inizio di: {nome_file}\n")
                unione.write(singolo.read())
                unione.write(f"\n# Fine di: {nome_file}\n\n")
