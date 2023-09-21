import csv
import pathlib
import shutil


def build_words_table():
    headers = ('text', 'code', 'weight', 'comment')
    with open('./meta.tsv') as meta_file, open('./meta.special.tsv') as meta_sp_file, open('./out/words.dict.tsv', 'w') as output_file:
        meta_special = dict()
        meta_special_reader = csv.DictReader(meta_sp_file, delimiter='\t')
        for row in meta_special_reader:
            meta_special[row['text']] = row['code']

        meta_reader = csv.DictReader(meta_file, delimiter='\t')
        writer = csv.writer(output_file, delimiter='\t', lineterminator='\n')
        writer.writerow(headers)
        for row in meta_reader:
            is_regular = row['分类'] == 'CJK'
            if is_regular:
                big_code = ''.join(c for c in row['编码'] if c.isupper()).lower()
                writer.writerow((row['文字'], big_code, row['权重'], row['拆分']))

                special_code = meta_special.get(row['文字'])
                if special_code is not None and (not big_code.startswith(special_code)):
                    writer.writerow((row['文字'], special_code, row['权重'], row['拆分']))


def build_phrases_table():
    shutil.copy('./meta.special.phrases.tsv', './out/phrases.dict.tsv')

if __name__ == '__main__':
    pathlib.Path("./out").mkdir(exist_ok=True)
    build_words_table()
    build_phrases_table()
