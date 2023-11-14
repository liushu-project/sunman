import csv
import pathlib
import shutil
import re


def build_words_table():
    special_words = get_special_words_table()
    with open('./data/拆分.tsv') as meta_file, open('./data/单字.tsv') as weight_file:
        weight = {}
        weight_reader = csv.DictReader(weight_file, delimiter='\t')
        for row in weight_reader:
            weight[row['文字']] = row['权重']

        table = dict()
        table_reader = csv.DictReader(decomment(meta_file), delimiter='\t')
        for row in table_reader:
            meta = row['meta']
            scope = re.search(r'『(.*)』', meta).group(1)

            if scope == 'CJK':
                comment = re.search(r'〔(.*)〕', meta).group(1)
                code = re.search(r'【(.*)】', meta).group(1)
                text = row['text']
                if text in table:
                    table[text]['codes'].add(code)
                else:
                    data = {
                        'codes': { code, },
                        'comment': comment,
                        'weight': weight[text]
                    }
                    table[text] = data

        with open('./out/words.dict.tsv', 'w') as output_file:
            headers = ('text', 'code', 'weight', 'comment')
            writer = csv.writer(output_file, delimiter='\t', lineterminator='\n')
            writer.writerow(headers)
            for key, value in table.items():
                codes = value['codes']
                special_code = special_words.get(key)
                for code in codes:
                    if any(code in x for x in codes if code != x):
                        # 忽略完全为其他码的前缀的码
                        pass
                    else:
                        if key == '你':
                            print(f"{value} code: {codes}")
                        weight = value.get('weight', 0)
                        # 存在特码且不被当前码包含
                        if special_code is not None and len(special_code) <= len(code) and not code.startswith(special_code):
                            writer.writerow((key, special_code, weight, value['comment']))
                            # 当前码权重归0
                            weight = 0
                        writer.writerow((key, code, weight, value['comment']))

def get_special_words_table():
    result = {}
    with open('./data/特码字.tsv') as file:
        reader = csv.DictReader(file, delimiter='\t')
        for row in reader:
            result[row['text']] = row['code']
    return result

def build_phrases_table():
    shutil.copy('./data/特码词.tsv', './out/phrases.dict.tsv')

def is_comment(line):
    return line.startswith('#')

def is_whitespace(line):
    return line.isspace()

def decomment(csvfile):
    for row in csvfile:
        if is_comment(row) == False and is_whitespace(row) == False:
            yield row

if __name__ == '__main__':
    pathlib.Path("./out").mkdir(exist_ok=True)
    build_words_table()
    build_phrases_table()
