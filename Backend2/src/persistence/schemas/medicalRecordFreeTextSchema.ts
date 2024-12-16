import mongoose from 'mongoose';
let Schema = mongoose.Schema;

const MedicalRecordFreeTextSchema =  new mongoose.Schema(
  {
    domainId: {type: Number, unique: true, required: true},
    doctorId: {type: String, required: true},
    comment: {type: String, required: false},
    medicalRecordId: {type: Schema.Types.ObjectId, ref: 'MedicalRecord', required: true}
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IMedicalRecordFreeTextPersistence & mongoose.Document>('MedicalRecordFreeText', MedicalRecordFreeTextSchema);
