import mongoose from 'mongoose';
import {IMedicalRecordAllergyPersistence} from "../../dataschema/IMedicalRecordAllergyPersistence";
let Schema = mongoose.Schema;
const MedicalRecordAllergySchema =  new mongoose.Schema(
  {
    domainId: {type: Number, unique: true, required: true},
    allergy: {type:Schema.Types.ObjectId,ref: 'Allergy',required: true},
    doctorId: {type: String, required: true},
    comment: {type: String, required: false},
    medicalRecordId: {type: Schema.Types.ObjectId, ref: 'MedicalRecord', required: true}
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IMedicalRecordAllergyPersistence & mongoose.Document>('MedicalRecordAllergy', MedicalRecordAllergySchema);
