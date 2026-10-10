"""Inventory upstream definitions without reading knowledge answers."""

import argparse
import csv
import hashlib
import json
import subprocess
from collections import Counter
from pathlib import Path

import pyarrow
import pyarrow.parquet as pq


def digest(path):
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def git_output(root, *args):
    return subprocess.check_output(
        ["git", "-C", str(root), *args], text=True
    ).strip()


def build_inventory(source):
    manifest_path = source / "output/analysis/manifest.csv"
    with manifest_path.open(newline="") as stream:
        manifest = {row["table"]: row for row in csv.DictReader(stream)}
    table_names = (
        "analysis_items",
        "analysis_participants",
        "analysis_phase_participants",
    )
    inputs = {}
    for name in table_names:
        record = manifest[name]
        path = source / record["path"]
        checksum = digest(path)
        if checksum != record["sha256"]:
            raise ValueError(f"Upstream manifest mismatch: {name}")
        file = pq.ParquetFile(path)
        if file.metadata.num_rows != int(record["rows"]):
            raise ValueError(f"Upstream row-count mismatch: {name}")
        columns = file.schema_arrow.names
        if len(columns) != int(record["columns"]):
            raise ValueError(f"Upstream column-count mismatch: {name}")
        inputs[name] = {
            "path": record["path"],
            "sha256": checksum,
            "rows": file.metadata.num_rows,
            "columns": columns,
        }

    items = pq.read_table(
        source / inputs["analysis_items"]["path"]
    ).to_pylist()
    keys = [(row["poll_id"], row["item_id"]) for row in items]
    if len(keys) != len(set(keys)):
        raise ValueError("Nonunique canonical item keys")
    items.sort(key=lambda row: (row["poll_id"], row["item_id"]))
    item_counts = Counter(row["poll_id"] for row in items)
    counts = {}
    for name in table_names[1:]:
        table = pq.read_table(
            source / inputs[name]["path"],
            columns=["poll_id", "source_dataset"],
        )
        counts[name] = Counter(
            (row["poll_id"], row["source_dataset"])
            for row in table.to_pylist()
        )
    all_polls = sorted(
        set(item_counts).union(
            *(set(poll for poll, _ in value) for value in counts.values())
        )
    )
    coverage = []
    for poll in all_polls:
        row = {"poll_id": poll, "catalog_items": item_counts[poll]}
        for name, value in counts.items():
            row[name] = {
                dataset: number
                for (poll_id, dataset), number in sorted(value.items())
                if poll_id == poll
            }
        coverage.append(row)
    audit = {
        "source_head": git_output(source, "rev-parse", "HEAD"),
        "source_worktree_status": git_output(source, "status", "--short"),
        "manifest_sha256": digest(manifest_path),
        "pyarrow_version": pyarrow.__version__,
        "knowledge_answers_read": False,
        "inputs": inputs,
        "coverage": coverage,
        "candidate_cases_without_catalog_items": [
            poll for poll in ("bulgaria-2007",) if poll not in item_counts
        ],
    }
    inventory = {
        "source_items_sha256": inputs["analysis_items"]["sha256"],
        "status": "unreviewed_source_inventory_not_substantive_coding",
        "items": items,
    }
    return audit, inventory


def main():
    project = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--source", type=Path, default=project.parent / "dp_data"
    )
    parser.add_argument("--output", type=Path, default=project / "metadata")
    args = parser.parse_args()
    audit, inventory = build_inventory(args.source.resolve())
    args.output.mkdir(parents=True, exist_ok=True)
    for name, value in (
        ("input_audit.json", audit),
        ("item_inventory.json", inventory),
    ):
        (args.output / name).write_text(
            json.dumps(value, indent=2, ensure_ascii=False, allow_nan=False)
            + "\n"
        )
    print(
        f"Inventoried {len(inventory['items'])} items; "
        "no knowledge answers read."
    )


if __name__ == "__main__":
    main()
