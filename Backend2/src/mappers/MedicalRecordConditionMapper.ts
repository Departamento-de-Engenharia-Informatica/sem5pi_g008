import {Mapper} from "../core/infra/Mapper";
import mongoose, {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {MedicalRecordCondition} from "../domain/MedicalRecordCondition/MedicalRecordCondition";
import IMedicalRecordConditionDTO from "../dto/IMedicalRecordConditionDTO";
import {IMedicalRecordConditionPersistence} from "../dataschema/IMedicalRecordConditionPersistence";
import IStaffDetailsDTO from "../dto/IStaffDetailsDTO";

export class MedicalRecordConditionMapper extends Mapper<MedicalRecordCondition> {

  public static toDTO(domain: any, medicalConditionDesignation?: string, medicalConditionBusinessId?: string, medicalRecordBusinessId?: string, staffDetailsDTO?: IStaffDetailsDTO): IMedicalRecordConditionDTO {
    
    if (medicalConditionBusinessId && medicalConditionDesignation && medicalRecordBusinessId) {
      
      if(staffDetailsDTO) {
        return {
          conditionId: medicalConditionBusinessId.toString(),
          conditionDesignation: medicalConditionDesignation,
          medicalRecordId: medicalRecordBusinessId.toString(),
          doctorName: staffDetailsDTO.firstName + ' ' + staffDetailsDTO.lastName, 
          doctorLicenseNumber: staffDetailsDTO.licenseNumber,
          comment: domain.comment
        } as IMedicalRecordConditionDTO;
      }
      
      return {
        conditionId: medicalConditionBusinessId.toString(),
        conditionDesignation: medicalConditionDesignation,
        medicalRecordId: medicalRecordBusinessId.toString(),
        doctorName: "Invalid",
        comment: domain.comment
      } as IMedicalRecordConditionDTO;
    }

    return {
      conditionId: domain.condition,
      medicalRecordId: domain.medicalRecord,
      doctorId: domain.doctorId,
      comment: domain.comment
    } as IMedicalRecordConditionDTO;
  }

  public static toDomain (medicalRecordCondition: any | Model<IMedicalRecordConditionPersistence & Document> ): MedicalRecordCondition {
    
    const medicalRecordConditionResult = MedicalRecordCondition.create(
      medicalRecordCondition,
      new UniqueEntityID(medicalRecordCondition.domainId)
    );

    medicalRecordConditionResult.isFailure ? console.log(medicalRecordConditionResult.error) : '';

    return medicalRecordConditionResult.isSuccess ? medicalRecordConditionResult.getValue() : null;
  }

  public static toPersistence (medicalRecordCondition: MedicalRecordCondition, id?: number): IMedicalRecordConditionPersistence {
    if(id === undefined) {
      return {
        domainId: medicalRecordCondition.domainId,
        conditionId: new mongoose.Types.ObjectId(medicalRecordCondition.condition),
        medicalRecordId: new mongoose.Types.ObjectId(medicalRecordCondition.medicalRecord),
        comment: medicalRecordCondition.comment,
        doctorId: medicalRecordCondition.doctorId,
      }
    }

    return {
      domainId: id,
      conditionId: new mongoose.Types.ObjectId(medicalRecordCondition.condition),
      medicalRecordId: new mongoose.Types.ObjectId(medicalRecordCondition.medicalRecord),
      comment: medicalRecordCondition.comment,
      doctorId: medicalRecordCondition.doctorId,
    }
  }
  
  
}
