import IMedicalRecordDTO from "../../dto/IMedicalRecordDTO";

export default interface IMedicalRecordService{
    updateMedicalRecord(medicalRecord: IMedicalRecordDTO): Promise<any>;
    
    createMedicalRecord(medicalRecord: IMedicalRecordDTO): Promise<any>;
    
    
}