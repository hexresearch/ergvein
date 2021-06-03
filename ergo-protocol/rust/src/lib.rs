extern crate ergo_lib;
extern crate ergotree_ir;
extern crate sigma_ser;

use ergo_lib::chain::{ergo_box::BoxId, transaction::Transaction};
use ergotree_ir::ergo_tree::ErgoTree;
use ergotree_ir::serialization::{SerializationError, SigmaSerializable, constant_store::ConstantStore, sigma_byte_reader::SigmaByteReader};
use sigma_ser::{peekable_reader::PeekableReader, vlq_encode::{ReadSigmaVlqExt, VlqEncodingError}};
use std::io::{self, Cursor};
use std::ptr;

pub struct BlockReader {
    internal: SigmaByteReader<PeekableReader<Cursor<Vec<u8>>>>,
}

#[no_mangle]
pub extern "C" fn ergo_decode_block_body(
  raw: *const u8,
  len: u64,
) -> Box<BlockReader> {
    let block = unsafe { std::slice::from_raw_parts(raw, len as usize) }.to_owned();
    let cursor = Cursor::new(block);
    let peekable = PeekableReader::new(cursor);
    let reader = SigmaByteReader::new(peekable, ConstantStore::empty());
    Box::new(BlockReader { internal: reader })
}

#[no_mangle]
pub extern "C" fn ergo_free_block_reader(reader: Box<BlockReader>) {
    drop(reader)
}

#[no_mangle]
pub extern "C" fn ergo_peek_block_length(reader: Box<BlockReader>, result: *mut u32) -> u32 {

    fn internal(mut reader: Box<BlockReader>) -> Result<u32, VlqEncodingError> {
        let n = reader.internal.get_u32()?;
        if n == 10000002 { reader.internal.get_u32() } else { Ok(n) }
    }

    match internal(reader) {
        Ok(v) => {
            unsafe { *result = v; }
            0
        }
        Err(e) => {
            println!("Failed to parse block length: {:?}", e);
            1
        }
    }
}

#[no_mangle]
pub extern "C" fn ergo_peek_next_transaction(mut reader: Box<BlockReader>, result: *mut Box<Transaction>) -> u32 {
    match Transaction::sigma_parse(&mut reader.internal) {
        Ok(v) => {
            unsafe { *result = Box::new(v); }
            0
        }
        Err(e) => {
            println!("Failed to parse transaction: {:?}", e);
            1
        }
    }
}

#[no_mangle]
pub extern "C" fn ergo_tx_inputs(tx: Box<Transaction>) -> u32 {
    tx.inputs.len() as u32
}

#[no_mangle]
pub extern "C" fn ergo_tx_data_inputs(tx: Box<Transaction>) -> u32 {
    tx.data_inputs.len() as u32
}

#[no_mangle]
pub extern "C" fn ergo_tx_output_candidates(tx: Box<Transaction>) -> u32 {
    tx.output_candidates.len() as u32
}

#[no_mangle]
pub extern "C" fn ergo_tx_input_boxid(tx: Box<Transaction>, i: u32, result: *mut u8) -> u32 {
    let len = tx.inputs.len() as u32;
    if i >= len {
        println!("Out of range id {} for tx inputs (length {})", i, len);
        return 1;
    }
    let bytes = tx.inputs[i as usize].box_id.sigma_serialize_bytes();
    unsafe {
        ptr::copy(bytes.as_ptr(), result, bytes.len());
    }
    0
}
