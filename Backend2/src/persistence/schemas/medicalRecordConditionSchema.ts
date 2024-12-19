import mongoose from 'mongoose';
import {IMedicalRecordConditionPersistence} from "../../dataschema/IMedicalRecordConditionPersistence";
let Schema = mongoose.Schema;
const MedicalRecordConditionSchema = new mongoose.Schema(
  {
    domainId: {type: Number, unique: true, required: true},
    conditionId: {type:Schema.Types.ObjectId,ref: 'MedicalCondition',required: true},
    medicalRecordId: {type: Schema.Types.ObjectId, ref: 'MedicalRecord', required: true},
    doctorId: {type: String, required: true},
    comment: {type: String, required: false}
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IMedicalRecordConditionPersistence & mongoose.Document>('MedicalRecordCondition', MedicalRecordConditionSchema);
