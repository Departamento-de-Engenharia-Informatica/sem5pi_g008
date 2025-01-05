import mongoose from 'mongoose';
import {IMedicalRecordFreeTextPersistence} from "../../dataschema/IMedicalRecordFreeTextPersistence";
let Schema = mongoose.Schema;

const MedicalRecordFreeTextSchema =  new mongoose.Schema(
  {
    domainId: {type: Number, unique: true, required: true},
    medicalRecordId: {type: Schema.Types.ObjectId, ref: 'MedicalRecord', required: true},
    doctorId: {type: String, required: true},
    comment: {type: String, required: false}
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IMedicalRecordFreeTextPersistence & mongoose.Document>('MedicalRecordFreeText', MedicalRecordFreeTextSchema);
