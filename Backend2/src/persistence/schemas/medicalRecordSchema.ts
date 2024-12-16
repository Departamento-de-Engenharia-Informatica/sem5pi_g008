// MedicalRecord.ts
import mongoose, { Schema, Document } from 'mongoose';
import {IMedicalRecordPersistence} from "../../dataschema/IMedicalRecordPersistence";

const MedicalRecordSchema = new Schema(
  {
    domainId: { type: String, required: true },
  },
  { timestamps: true }
);

export default mongoose.model<IMedicalRecordPersistence & Document>('MedicalRecord', MedicalRecordSchema);
