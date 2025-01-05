import {Mapper} from "../core/infra/Mapper";
import {MedicalRecordFamilyHistory} from "../domain/MedicalRecordFamilyHistory/MedicalRecordFamilyHistory";
import mongoose, {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {IMedicalRecordFamilyHistoryPersistence} from "../dataschema/IMedicalRecordFamilyHistoryPersistence";
import {IMedicalRecordFamilyHistoryDTO} from "../dto/IMedicalRecordFamilyHistoryDTO";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";

export class MedicalRecordFamilyHistoryMap extends Mapper<MedicalRecordFamilyHistory> {

    // Convert the domain object to DTO
    public static toDTO(domain: MedicalRecordFamilyHistory): IMedicalRecordFamilyHistoryDTO {
        return {
            medicalRecord: domain.medicalRecord,
            familyMember: domain.familyMember,
            condition: domain.condition,
        } as IMedicalRecordFamilyHistoryDTO;
    }

    // Convert the persistence object (e.g., from MongoDB) to the domain object
    public static toDomain(medicalRecordFamilyHistory: any | Model<IMedicalRecordFamilyHistoryPersistence & Document>): MedicalRecordFamilyHistory {
        const medicalRecordFamilyHistoryResult = MedicalRecordFamilyHistory.create({
            medicalRecord: medicalRecordFamilyHistory.medicalRecord,
            familyMember: medicalRecordFamilyHistory.familyMember,
            condition: medicalRecordFamilyHistory.condition,

        }, new UniqueEntityID(medicalRecordFamilyHistory.domainId));

        // Log error if creation fails
        medicalRecordFamilyHistoryResult.isFailure ? console.log(medicalRecordFamilyHistoryResult.error) : '';

        // Return the domain object or null if failure
        return medicalRecordFamilyHistoryResult.isSuccess ? medicalRecordFamilyHistoryResult.getValue() : null;
    }
 
    // Convert the domain object to persistence format (e.g., for MongoDB storage)


   public static toPersistence(familylist: any, id: any) {
        const rawFamilyHistory: any = {
            domainId: id,
            medicalRecordId: familylist.medicalRecordId,
            familyMember: familylist.familyMember,
            condition: familylist.condition,
        };
        return rawFamilyHistory;
        
    }
}
